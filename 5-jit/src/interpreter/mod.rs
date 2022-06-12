use std::collections::{BTreeMap, HashMap};
use std::num::NonZeroU64;
use itertools::Itertools;
use itertools::Either;
use itertools::Either::{Left, Right};
use anyhow::Result;
use anyhow::anyhow;

use crate::*;
use crate::jit::CompiledChunk;
use bc::*;
use heap::*;
use util::BooleanAssertions;
use smallvec::smallvec;

const STACK_LIMIT: usize = 1024;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackFrame {
    // for a method, this is start + length
    // used to check if we're past the local code vector
    code_end: Pc,
    locals: Vec<Value>,
    return_address: Pc,
}

pub struct Interpreter {
    pub constant_pool: Vec<Constant>,
    pub label_map: HashMap<String, Pc>,
    /// the values are both values and constant pool indices,
    /// depending on whether the global is a variable or a function
    pub global_map: HashMap<u16, Either<Value, u16>>,
    pub code: Vec<Instr>,
    pub stack: Vec<Value>,
    pub call_stack: Vec<StackFrame>,
    pub should_gc: bool,
    pub heap: Heap,
    pub pc: Pc,
    pub jit_enabled: bool,
    pub compilation_cache: HashMap<u16, (u8, Option<CompiledChunk>)>
}

impl Interpreter {
    pub fn load<B: std::io::Read>(
        buf: &mut B, heap_log: Option<String>, heap_size: Option<NonZeroU64>, jit_enabled: bool
    ) -> Result<Interpreter> {
        let mut input = Vec::new();
        buf.read_to_end(&mut input)?;
        let mut iter = input.iter();
        let (mut constant_pool, code) = bc::parse_constant_pool(&mut iter)?;
        let globals = bc::parse_globals(&mut iter)?;
        let entry_point = bc::parse_entry_point(&mut iter)? as usize;
        let (code, label_map) =
            bc::collect_labels(&mut constant_pool, code)?;

        let (pc, length, init_locals) = match &constant_pool[entry_point] {
            Constant::Method { name_idx: _, n_args: _, n_locals, start, length } =>
                Ok((*start, *length, *n_locals)),
            k => Err(anyhow!("Invalid entry point {:?}", k)),
        }?;

        let mut init = Interpreter {
            stack: vec![],
            call_stack: vec![],
            heap: Heap::with_capacity(heap_size),
            global_map: Default::default(),
            should_gc: false,
            constant_pool,
            label_map,
            code,
            pc,
            jit_enabled,
            compilation_cache: Default::default(),
        };

        init.heap.log = heap_log;

        let bottom_frame = StackFrame {
            code_end: pc + length,
            locals: (0..init_locals).map(|_| Value::Null).collect(),
            return_address: 0,
        };

        init.call_stack.push(bottom_frame);
        init.init_with_globals(globals)
    }

    pub fn init_with_globals(self, globals: Vec<u16>) -> Result<Interpreter> {
        let mut global_map = HashMap::new();
        for &k_idx in globals.iter() {
            self.constant_pool.get(k_idx as usize)
                .ok_or_else(|| anyhow!("invalid global, {} is outside the constant pool", k_idx))
                .cloned()
                .and_then(|k| match k {
                    Constant::Slot(i) => {
                        self.constant_pool.get(i as usize)
                            .ok_or_else(|| anyhow!("invalid global, {} is outside the constant pool", i))
                            .and_then(Constant::as_string)?;
                        global_map.insert(i, Left(Value::Null));
                        Ok(())
                    },
                    Constant::Method { name_idx, .. } => self.constant_pool[name_idx as usize]
                        .as_string()
                        .map(|_| {
                            global_map.insert(name_idx, Right(k_idx));
                        }),
                    _ => Err(anyhow!("Invalid global {:?}", k)),
                })?;
        }

        Ok(Interpreter { global_map, ..self })
    }

    pub fn frame(&mut self) -> &mut StackFrame {
        self.call_stack.last_mut().unwrap()
    }

    pub fn push(&mut self, value: Value) -> Result<()> {
        if self.stack.len() >= STACK_LIMIT {
            Err(anyhow!("stack overflow"))
        } else {
            self.stack.push(value);
            Ok(())
        }
    }

    pub fn pop(&mut self) -> Result<Value> {
        self.stack.pop().ok_or_else(|| anyhow!("stack underflow"))
    }

    pub fn peek(&self) -> Result<Value> {
        self.stack.last().cloned().ok_or_else(|| anyhow!("stack underflow"))
    }

    pub fn alloc(&mut self, obj: &HeapObject) -> Result<Pointer> {
        if self.heap.should_gc_before_alloc(obj) {
            self.should_gc = true;
        }

        self.heap.alloc_after_gc(obj)
    }

    pub fn gc(&mut self) -> Result<()> {
        let rename = self.heap.gc(self.stack.iter().cloned()
            .chain(self.global_map.values()
                .filter_map(|v| v.clone().left())
            )
            .chain(
                self.call_stack.iter().flat_map(|frame| frame.locals.iter().cloned())
            ).filter_map(|v| match v {
                Value::Reference(p) => Some(p),
                _ => None,
            })
            .unique()
            .collect_vec()
        )?;
        self.stack.iter_mut()
        .chain(self.call_stack.iter_mut().flat_map(|frame| frame.locals.iter_mut()))
        .for_each(|v| if let Value::Reference(p) = v {
            *p = rename[p];
        });
        self.global_map.iter_mut().for_each(|(_k, v)| if let Left(Value::Reference(p)) = v {
            *p = rename[p];
        });
        Ok(())
    }

    pub fn execute(&mut self) -> Result<()> {
        match run(self) {
            Ok(()) => Ok(()),
            Err(e) => {
                let Interpreter {
                    call_stack,
                    code,
                    constant_pool,
                    global_map,
                    label_map,
                    should_gc,
                    pc,
                    stack,
                    heap: _,
                    jit_enabled,
                    compilation_cache,
                } = self;
                eprintln!(
                    "interpreter crash:\n\
                    call_stack = {call_stack:#?}\n\
                    code = {code:?}\n\
                    constant_pool = {constant_pool:?}\n\
                    global_map = {global_map:#?}\n\
                    label_map = {label_map:#?}\n\
                    should_gc = {should_gc:#?}\n\
                    pc = {pc:#?}\n\
                    jit_enabled = {jit_enabled}\n\
                    stack = {stack:#?}\n\
                    compilation_cache = {compilation_cache:#?}\n\
                    ",
                    call_stack = call_stack,
                    code = code,
                    constant_pool = constant_pool,
                    global_map = global_map,
                    label_map = label_map,
                    should_gc = should_gc,
                    pc = pc,
                    jit_enabled = jit_enabled,
                    stack = stack,
                    compilation_cache = compilation_cache,
                );
                Err(e)
            },
        }
    }

    pub fn perform_return(&mut self) -> Result<()> {
        let StackFrame {
            code_end: _,
            return_address,
            locals: _,
        } = self.call_stack.pop().unwrap();
        self.pc = return_address;
        Ok(())
    }

    /**
     * Call a method.
     * @warn Handles PC manipulation.
     */
    pub fn call_method(&mut self, receiver: Value, name: u16, args: Vec<Value>) -> Result<()> {
        match receiver {
            Value::Null | Value::Int(_) | Value::Bool(_) => {
                (args.len() == 1).expect(|| anyhow!(
                    "attempt to call {} on {} failed: \
                    primitive receivers only support methods \
                    of one argument, not {}",
                    name,
                    self.show(&receiver),
                    args.len()
                ))?;
                self.push(self.call_builtin_method(
                    receiver,
                    self.constant_pool[name as usize].as_str()?,
                    args[0].clone()
                )?)?;
                self.pc += 1;
                Ok(())
            },
            Value::Reference(ptr) => {
                match self.heap.read_tag(ptr)? {
                    HeapTag::Array(len) => self.call_array_method(name, &args, len as i32, ptr),
                    HeapTag::Object => self.call_object_method(ptr, name, args, receiver),
                }
            },
        }
    }

    pub fn call_object_method(&mut self, ptr: Pointer, name: u16, mut args: Vec<Value>, receiver: Value) -> Result<()> {
        let (parent, methods) = {
            match self.heap.read(ptr)? {
                HeapObject::Object { parent, methods, fields: _ } => (parent, methods),
                _ => unreachable!(),
            }
        };

        let index = methods.binary_search_by_key(&name, |p| p.0).ok();
        match index {
            Some(i) => {
                let method_idx = methods[i].1 as usize;
                let (_name, n_args, n_locals, start, len) = self.constant_pool[method_idx].as_method()?;
                (n_args as usize == args.len() + 1).expect(|| anyhow!(
                    "wrong number of arguments, {} expects {}, not {} (receiver and {:?})", name, n_args, args.len() + 1, args
                ))?;
                args.push(receiver);
                self.call_function(method_idx as u16, start, start + len, n_locals, &args)
            },
            None => self.call_method(parent, name, args),
        }
    }

    pub fn call_array_method(&mut self, name: u16, args: &[Value], len: i32, ptr: Pointer) -> Result<()> {
        match (self.constant_pool[name as usize].as_str()?, args[0].clone(), args.get(1)) {
            ("get", Value::Int(i), _) => {
                (i >= 0 && i < len).expect(|| anyhow!("array index {} out of bounds", i))?;
                let v = self.heap.array_get(ptr, i as u32)?;
                self.push(v)?;
            },
            ("set", v, Some(&Value::Int(i))) => {
                (i >= 0 && i < len).expect(|| anyhow!("array index {} out of bounds", i))?;
                self.heap.array_set(ptr, i as u32, v.clone())?;
                self.push(v)?;
            },
            ("get" | "set", v, _) => return Err(anyhow!(
                "indexing an array with a {} might work in PHP, but it won't work here.", self.show(&v)
            )),
            (invalid, _, _) => return Err(anyhow!(
                "this is actually the very first time someone thought to call {} on an array.", invalid
            )),
        };
        self.pc += 1;
        Ok(())
    }

    pub fn show(&self, v: &Value) -> String {
        fn go_obj(this: &Interpreter, depth: u8, obj: &HeapObject) -> String {
            match obj {
                HeapObject::Array(arr) => format!("[{}]",
                    arr.iter().map(|el| go(this, depth + 1, el)).collect_vec().join(", ")
                ),
                HeapObject::Object { parent, fields, methods: _ } => format!(
                    "object({})",
                    (match parent {
                        Value::Null => None,
                        obj => Some(format!("..={}", go(this, depth + 1, obj))),
                    }).into_iter().chain(fields.iter()
                        .sorted_by_key(|f| this.constant_pool[f.0 as usize].as_string().unwrap())
                        .map(|(k, v)| format!(
                        "{}={}",
                        this.constant_pool[*k as usize].as_string().unwrap(),
                        go(this, depth + 1, v)
                    )))
                    .collect_vec()
                    .join(", ")
                ),
            }
        }

        fn go(this: &Interpreter, depth: u8, v: &Value) -> String {
            if depth > 10 { return "[deep!]".to_string(); }
            match v {
                Value::Null => "null".to_string(),
                Value::Int(n) => n.to_string(),
                Value::Bool(b) => b.to_string(),
                &Value::Reference(ptr) => go_obj(this, depth + 1, &this.heap.read(ptr).unwrap()),
            }
        }

        go(self, 0, v)
    }

    pub fn call_builtin_method(&self, receiver: Value, name: &str, arg: Value) -> Result<Value> {
        use Value::*;
        let unsupported = |name, r, arg|
            anyhow!("unsupported builtin method {} for receiver {} with arg {}", name, self.show(&r), self.show(&arg));

            match (name, receiver, arg) {
            ("+"  | "add", Int(a), Int(b)) => Ok(Int(a + b)),
            ("-"  | "sub", Int(a), Int(b)) => Ok(Int(a - b)),
            ("*"  | "mul", Int(a), Int(b)) => Ok(Int(a * b)),
            ("/"  | "div", Int(a), Int(b)) => Ok(Int(a / b)),
            ("%"  | "mod", Int(a), Int(b)) => Ok(Int(a % b)),
            ("<"  | "lt",  Int(a), Int(b)) => Ok(Bool(a < b)),
            (">"  | "gt",  Int(a), Int(b)) => Ok(Bool(a > b)),
            ("<=" | "le",  Int(a), Int(b)) => Ok(Bool(a <= b)),
            (">=" | "ge",  Int(a), Int(b)) => Ok(Bool(a >= b)),
            ("&"  | "and", Bool(a), Bool(b)) => Ok(Bool(a && b)),
            ("|"  | "or",  Bool(a), Bool(b)) => Ok(Bool(a || b)),
            ("==" | "eq",  Int(a), Int(b)) => Ok(Bool(a == b)),
            ("==" | "eq",  Int(_), _) => Ok(Bool(false)),
            ("==" | "eq",  Bool(a), Bool(b)) => Ok(Bool(a == b)),
            ("==" | "eq",  Bool(_), _) => Ok(Bool(false)),
            ("==" | "eq",  Null, Null) => Ok(Bool(true)),
            ("==" | "eq",  Null, _) => Ok(Bool(false)),
            ("!=" | "neq", r, arg) =>
                // TODO avoid cloning
                self.call_builtin_method(r.clone(), "==", arg.clone()).map(
                    |bool_value| Bool(!bool_value.as_bool(|_| unreachable!()).unwrap())
                ).map_err(|_| unsupported(name, r, arg)),
            (_, r, arg) => Err(unsupported(name, r, arg)),
        }
    }

    pub fn call_function(&mut self, fn_addr: u16, start: Pc, end: Pc, n_locals: u16, args: &[Value]) -> Result<()> {
        self.call_stack.push(StackFrame {
            code_end: end,
            return_address: self.pc + 1,
            locals: args.iter().rev().chain((0..n_locals).map(|_| &Value::Null)).cloned().collect(),
        });
        match self.compilation_cache
            .entry(fn_addr)
            .and_modify(|(call_count, _chunk)|
                *call_count = call_count.saturating_add(1)
            )
            .or_insert((1, None)) {
            &mut (call_count, _) if call_count < jit::CALL_THRESHOLD || !self.jit_enabled => {
                // interpret by jumping to the start
                self.pc = start;
                Ok(())
            },
            (call_count, maybe_chunk@None) => {
                // we've hit the threshold, compile and call
                self.heap.trace(format!(
                    "compiling function {} to machine code, {} instructions (call count: {})",
                    fn_addr,
                    end - start,
                    *call_count,
                ));
                let (dbg, chunk) = jit::compile(start, &self.code[start..end])?;
                *maybe_chunk = Some(chunk);
                self.heap.trace(format!(
                    "compilation of function {} succeded:{}",
                    fn_addr,
                    dbg,
                ));
                unsafe { maybe_chunk.as_ref().unwrap().1(); }
                Ok(())
            },
            (_, Some(chunk)) => {
                // we have a compiled chunk already, just call it
                unsafe { chunk.1(); }
                Ok(())
            },
        }
    }

    pub fn raw_field(&mut self, ptr: Pointer, name: u16) -> Result<&mut [u8/*; 8*/]> {
        let str_name = self.constant_pool[name as usize].as_str().unwrap();
        // let refr = Value::Reference(ptr);

        match self.heap.get_field(ptr, name, str_name) {
            Ok(slice) => Ok(slice),
            // FIXME resolve borrowck issues and give the original error message
            Err(_) => Err(anyhow!(
                "cannot access field {} of ???. In fact, it doesn't even matter
                what the field's name is, as ??? is not an object.", str_name, //self.show(&refr), self.show(&refr)
            )),
        }
    }

    pub fn print(&mut self, k: u16, args: &[Value]) -> Result<Value> {
        let format = self.constant_pool[k as usize].as_str()?;
        let mut escape = false;
        let mut str = String::new();
        let mut err = None;
        let mut arg_index = 0;

        let escapes = BTreeMap::from_iter(vec![
            ('~', '~'), ('n', '\n'), ('"', '"'), ('r', '\r'), ('t', '\t'), ('\\', '\\')
        ]);

        for ch in format.chars() {
            match (escape, ch) {
                (false, '~') => match args.get(arg_index) {
                    Some(arg) => {
                        str.push_str(&self.show(arg));
                        arg_index += 1
                    },
                    None => {
                        err = Some("\
                            the little wavy symbols in your format string are trying to read \
                            the arguments that come after it. Try putting enough arguments in, \
                            see what happens.".to_string()
                        );
                        break
                    },
                },
                (false, '\\') => escape = true,
                (false, ch) => str.push(ch),
                (true, ch) if escapes.contains_key(&ch) => {
                    str.push(escapes[&ch]);
                    escape = false
                },
                (true, _) => {
                    err = Some(format!(
                        "you can try escaping {} as much as you like, it's not going to work.", ch
                    ));
                    break
                },
            }
        }

        match err {
            Some(err) => Err(anyhow!(err)),
            None if escape => Err(anyhow!(
                "honestly, backslashes at the end of format strings shouldn't even make it through the parser."
            )),
            None => {
                print!("{}", str);
                Ok(Value::Null)
            },
        }
    }

    pub fn create_array(&mut self) -> Result<()> {
        let initial = self.pop()?;
        let len = self.pop()?.as_int(
            |v| anyhow!("the length of an array must be an integer (not {})", self.show(v))
        )?;
        (len >= 0).expect(|| anyhow!("the length of an array must be non-negative"))?;
        let len = len as usize;
        let ptr = self.alloc(&HeapObject::Array(smallvec![initial; len]))?;
        self.push(Value::Reference(ptr))
    }

    pub fn create_object(&mut self, class: u16) -> Result<()> {
        let mut members = self.constant_pool[class as usize].as_class()?
            .into_iter()
            .map(|i| (i, self.constant_pool[i as usize].clone()))
            .map(|(i, k)| match k {
                Constant::Method { .. } => Ok((i as i16, k)),
                Constant::Slot(_) => Ok((-1, k)),
                _ => Err(anyhow!("an object cannot contain a {:?}", k)),
            })
            .collect::<Result<Vec<_>>>()?;
        members.sort_by_key(|p| p.0);
        members.reverse();
        let mut fields: ObjectFields = Default::default();
        let mut methods: ObjectMethods = Default::default();
        for (method_index, slot) in members.into_iter() {
            match slot {
                Constant::Slot(i) => fields.push((i, self.pop()?)),
                Constant::Method { name_idx, .. } => methods.push((name_idx, method_index as u16)),
                _ => unreachable!(),
            }
        }
        fields.sort_by_key(|p| p.0);
        methods.sort_by_key(|p| p.0);
        let parent = self.pop()?;
        let obj = HeapObject::Object {
            parent,
            fields,
            methods,
        };
        let ptr = self.alloc(&obj)?;
        self.push(Value::Reference(ptr))
    }

    pub fn perform_print(&mut self, k: u16, n_args: u8) -> Result<()> {
        let args = (0..n_args).map(|_| self.pop()).collect::<Result<Vec<_>>>()?;
        self.print(k, &args.into_iter().rev().collect_vec())?;
        self.push(Value::Null)
    }

    pub fn set_global(&mut self, name: u16) -> Result<()> {
        let v = self.peek()?;
        self.global_map.insert(name, Left(v));
        Ok(())
    }

    pub fn get_global(&mut self, name: u16) -> Result<()> {
        let v = self.global_map[&name].clone().unwrap_left();
        self.push(v)
    }

    pub fn set_local(&mut self, i: u16) -> Result<()> {
        let v = self.peek()?;
        self.frame().locals[i as usize] = v;
        Ok(())
    }

    pub fn get_local(&mut self, i: u16) -> Result<()> {
        let v = self.frame().locals[i as usize].clone();
        self.push(v)
    }

    pub fn set_field(&mut self, name: u16) -> Result<()> {
        let v = self.pop()?;
        let ptr = self.pop()?.as_reference(|_|
            anyhow!("attempt to set a field on a non-reference value")
        )?;
        let ref_bytes = self.raw_field(ptr, name)?;
        ref_bytes.copy_from_slice(v.to_le_bytes().as_slice());
        self.push(v)
    }

    pub fn get_field(&mut self, name: u16) -> Result<()> {
        let ptr = self.pop()?.as_reference(|_|
            anyhow!("attempt to get a field from a non-reference value")
        )?;
        let v = Value::from(self.raw_field(ptr, name)? as &[u8]);
        self.push(v)
    }

    pub fn perform_method_call(&mut self, i: u16, n_args: u8) -> Result<()> {
        let args = (0..n_args - 1).map(|_| self.pop()).collect::<Result<Vec<_>>>()?;
        let receiver = self.pop()?;
        self.call_method(receiver, i, args)
    }

    pub fn find_global_function(&self, global_name: &str, i: u16) -> Result<u16> {
        self.global_map.get(&i).cloned()
            .ok_or_else(|| anyhow!("compiler bug: global {} not found", global_name))?
            .right()
            .ok_or_else(|| anyhow!("attempt to call variable {}", global_name))
    }

    pub fn perform_function_call(&mut self, fn_addr: u16, n_args: u8) -> Result<()> {
        let args = (0..n_args)
            .map(|_| self.pop())
            .collect::<Result<Vec<_>>>()?;
        let (_, arity, n_locals, start, len) = self.constant_pool[fn_addr as usize].as_method()?;
        (arity == n_args).expect(||
            anyhow!("arity mismatch, expected {} arguments, got {}", arity, n_args)
        )?;
        self.call_function(fn_addr, start, start + len, n_locals, &args)
    }
}

fn run(this: &mut Interpreter) -> Result<()> {
    while !this.call_stack.is_empty() && this.pc < this.call_stack.last().unwrap().code_end {
        #[cfg(debug_assertions)]
        {
            println!("\tstack: {:?}", this.stack);
            print!("pc: {pc}, globals: ", pc = this.pc);
            for (name, target) in this.global_map.iter() {
                if let Left(v) = target {
                    print!("{}={} ", name, this.show(v));
                }
            }
            print!(" locals: ");
            for (i, addr) in this.frame().locals.clone().into_iter().enumerate() {
                print!("[{}]={} ", i, this.show(&addr));
            }
            println!("i: {instr:?}", instr = this.code[this.pc]);
        }

        if this.should_gc {
            this.gc()?;
            this.should_gc = false;
        }

        let mut increment = true;
        match this.code[this.pc].clone() {
            Instr::Literal(i) => {
                // replace with a specialised instruction
                increment = false;
                this.code[this.pc] = match this.constant_pool[i as usize] {
                    Constant::Null => Ok(Instr::LiteralNull),
                    Constant::Boolean(b) => Ok(Instr::LiteralBool(b)),
                    Constant::Integer(n) => Ok(Instr::LiteralInt(n)),
                    Constant::String(_) => Err(anyhow!("attempt to push a string onto the stack")),
                    Constant::Slot(_) => Err(anyhow!("attempt to push a slot onto the stack")),
                    Constant::Method { .. } => Err(anyhow!("attempt to push a method onto the stack")),
                    Constant::Class { .. } => Err(anyhow!("attempt to push a class onto the stack")),
                }?;
                Ok(()) as Result<()>
            },
            Instr::LiteralNull => {
                this.push(Value::Null)?;
                Ok(())
            },
            Instr::LiteralBool(b) => {
                this.push(Value::Bool(b))?;
                Ok(())
            },
            Instr::LiteralInt(n) => {
                this.push(Value::Int(n))?;
                Ok(())
            },
            Instr::Drop => {
                this.pop()?;
                Ok(())
            },
            Instr::Print(k, n_args) => this.perform_print(k, n_args),
            Instr::GetLocal(i) => this.get_local(i),
            Instr::SetLocal(i) => this.set_local(i),
            Instr::GetGlobal(i) => {
                // replace with a direct dereference
                increment = false;
                // let name = this.constant_pool[i as usize].as_string()?;
                this.code[this.pc] = Instr::GetGlobalDirect(i);
                Ok(())
            },
            Instr::GetGlobalDirect(name) => this.get_global(name),
            Instr::SetGlobal(i) => {
                // replace with a direct set
                increment = false;
                // let name = this.constant_pool[i as usize].as_string()?;
                this.code[this.pc] = Instr::SetGlobalDirect(i);
                Ok(())
            },
            Instr::SetGlobalDirect(name) => this.set_global(name),
            Instr::GetField(i) => {
                // replace with a direct get
                increment = false;
                // let name = this.constant_pool[i as usize].as_string()?;
                this.code[this.pc] = Instr::GetFieldDirect(i);
                Ok(())
            },
            Instr::GetFieldDirect(name) => this.get_field(name),
            Instr::SetField(i) => {
                // replace with a direct set
                increment = false;
                // let name = this.constant_pool[i as usize].as_string()?;
                this.code[this.pc] = Instr::SetFieldDirect(i);
                Ok(())
            },
            Instr::SetFieldDirect(name) => this.set_field(name),
            Instr::Label(_) => unreachable!("labels should be removed by the label collection pass"),
            Instr::Jump(i) => {
                // replace with a direct jump
                increment = false;
                let label = this.constant_pool[i as usize].as_string()?;
                let addr = this.label_map[&label];
                this.code[this.pc] = Instr::JumpDirect(addr);
                Ok(())
            },
            Instr::JumpDirect(target) => {
                increment = false;
                this.pc = target;
                Ok(())
            },
            Instr::Branch(i) => {
                // replace with a direct branch
                increment = false;
                let label = this.constant_pool[i as usize].as_string()?;
                let addr = this.label_map[&label];
                this.code[this.pc] = Instr::BranchDirect(addr);
                Ok(())
            },
            Instr::BranchDirect(target) => {
                let v = this.pop()?;
                if v.is_truthy() {
                    increment = false;
                    this.pc = target;
                }
                Ok(())
            },
            Instr::CallMethod(i, n_args) => {
                increment = false;
                this.perform_method_call(i, n_args)
            },
            Instr::CallFunction(i, n_args) => {
                match this.constant_pool[i as usize].as_str()? {
                    "force_gc" =>
                        if n_args != 0 {
                            Err(anyhow!("force_gc() doesn't take any arguments"))
                        } else {
                            this.gc()?;
                            this.push(Value::Null)
                        },
                    global_name => {
                        increment = false;
                        let fn_addr = this.find_global_function(global_name, i)?;
                        this.perform_function_call(fn_addr, n_args)
                    },
                }
            },
            Instr::Return => {
                increment = false;
                this.perform_return()
            },
            Instr::Array => this.create_array(),
            Instr::Object(class) => this.create_object(class),
        }?;

        this.pc += increment as usize;
    };
    Ok(())
}

#[cfg(test)]
mod test {
    use std::io::Cursor;
    use super::*;

    fn dummy() -> Interpreter {
        // dummy buffer
        let data = vec![
            1, 0, // 1 constant
            3, 0, 0, 0, 0, 0, 0, 0, 0, 0, // method
            0, 0, // no globals
            0, 0, // entry point
        ];
        let mut buf = Cursor::new(data);
        Interpreter::load(&mut buf, None, None, false).unwrap()
    }

    #[test]
    fn arrays() -> Result<()> {
        use HeapObject::Array;
        use Value::*;

        let mut i = dummy();
        let arr = i.alloc(&Array(smallvec![Null; 10]))?;
        let mut h = i.heap;

        let int1 = Int(1);
        let int2 = Int(-2);
        let int3 = Int(3);
        let int4 = Int(4);

        h.array_set(arr, 1, int2)?;
        h.array_set(arr, 0, int1)?;
        h.array_set(arr, 2, int3)?;
        h.array_set(arr, 9, int4)?;

        assert_eq!(h.array_get(arr, 0)?, Int(1));
        assert_eq!(h.array_get(arr, 1)?, Int(-2));
        assert_eq!(h.array_get(arr, 2)?, Int(3));
        assert_eq!(h.array_get(arr, 9)?, Int(4));

        Ok(())
    }
}
