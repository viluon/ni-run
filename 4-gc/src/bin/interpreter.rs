#![feature(fn_traits, associated_type_defaults)]

use std::collections::{BTreeMap, HashMap};
use fml_gc::util::BooleanAssertions;
use itertools::Itertools;
use itertools::Either;
use itertools::Either::{Left, Right};
use anyhow::Result;
use anyhow::anyhow;

use fml_gc::*;
use bc::*;
use heap::*;

const STACK_LIMIT: usize = 1024;

#[derive(Debug, Clone, PartialEq, Eq)]
struct StackFrame {
    // for a method, this is start + length
    // used to check if we're past the local code vector
    code_end: Pc,
    locals: Vec<usize>,
    return_address: Pc,
}

#[derive(Default)]
struct Interpreter {
    constant_pool: Vec<Constant>,
    label_map: HashMap<String, Pc>,
    // the values are both heap addresses and constant pool indices,
    // depending on whether the global is a variable or a function
    global_map: HashMap<u16, Either<usize, u16>>,
    code: Vec<Instr>,
    stack: Vec<Value>,
    call_stack: Vec<StackFrame>,
    heap: Vec<u8>,
    pc: Pc,
}

impl Interpreter {
    fn load<B: std::io::Read>(buf: &mut B) -> Result<Interpreter> {
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
            heap: vec![],
            global_map: Default::default(),
            constant_pool,
            label_map,
            code,
            pc,
        };

        let bottom_frame = StackFrame {
            code_end: pc + length,
            locals: (0..init_locals).map(|_| init.alloc(&Value::Null)).collect::<Result<_>>()?,
            return_address: 0,
        };

        init.call_stack.push(bottom_frame);
        init.init_with_globals(globals)
    }

    fn init_with_globals(mut self, globals: Vec<u16>) -> Result<Interpreter> {
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
                        global_map.insert(i, Left(self.alloc(&Value::Null)?));
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

    fn alloc(&mut self, v: &Value) -> Result<usize> {
        let addr = self.heap.len();
        self.set(addr, v)?;
        Ok(addr)
    }

    fn get(&self, addr: usize) -> Result<Value> {
        if addr < self.heap.len() {
            Ok(Value::from(&self.heap[addr..]))
        } else { Err(anyhow!("invalid address: {:#06x}", addr)) }
    }

    fn set(&mut self, addr: usize, v: &Value) -> Result<()> {
        let repr: Vec<u8> = v.into();
        if self.heap.len() < addr + repr.len() {
            self.heap.resize(addr + repr.len(), 0);
        }
        self.heap[addr..addr + repr.len()].copy_from_slice(repr.as_slice());
        Ok(())
    }

    fn frame(&mut self) -> &mut StackFrame {
        self.call_stack.last_mut().unwrap()
    }

    fn push(&mut self, value: Value) -> Result<()> {
        if self.stack.len() >= STACK_LIMIT {
            Err(anyhow!("stack overflow"))
        } else {
            self.stack.push(value);
            Ok(())
        }
    }

    fn pop(&mut self) -> Result<Value> {
        self.stack.pop().ok_or_else(|| anyhow!("stack underflow"))
    }

    fn peek(&self) -> Result<Value> {
        self.stack.last().cloned().ok_or_else(|| anyhow!("stack underflow"))
    }

    fn execute(&mut self) -> Result<()> {
        run(self)
    }

    fn perform_return(&mut self) -> Result<()> {
        let StackFrame {
            code_end: _,
            return_address,
            locals: _,
        } = self.call_stack.pop().unwrap();
        self.pc = return_address;
        Ok(())
    }

    fn array_set(&mut self, arr: usize, i: u32, el: u64) -> Result<()> {
        let heap = self.heap.as_mut_slice();
        let offset = arr + 1 + 8 + i as usize * 8;
        (offset + 8 <= heap.len()).expect(|| anyhow!("array index out of bounds"))?;
        heap[offset .. offset + 8].copy_from_slice(&el.to_le_bytes());
        Ok(())
    }

    fn array_get(&self, arr: usize, i: u32) -> Result<u64> {
        let heap = self.heap.as_slice();
        let offset = arr + 1 + 8 + i as usize * 8;
        (offset + 8 <= heap.len()).expect(|| anyhow!("array index out of bounds"))?;
        Ok(u64::from_le_bytes(heap[offset .. offset + 8].try_into().unwrap()))
    }

    fn deref(&self, mut v: Value) -> Result<(u64, Value)> {
        let mut array_addr = None;
        while let Value::Reference(addr) = v {
            array_addr = Some(addr);
            v = self.get(addr as usize)?
        }
        Ok((array_addr.ok_or_else(|| anyhow!("{:?} is not a reference!", v))?, v))
    }

    /**
     * Call a method.
     * @warn Handles PC manipulation.
     */
    fn call_method(&mut self, receiver: Value, name: u16, args: &[Value]) -> Result<()> {
        let str_name = self.constant_pool[name as usize].as_str()?;
        if matches!(receiver, Value::Int(_) | Value::Bool(_) | Value::Null) {
            (args.len() == 1).expect(|| anyhow!(
                "attempt to call {} on {} failed: \
                primitive receivers only support methods \
                of one argument, not {}",
                name,
                self.show(&receiver),
                args.len()
            ))?;
            self.push(self.call_builtin_method(receiver, str_name, args[0].clone())?)?;
            self.pc += 1;
            return Ok(());
        }

        let dereferenced = self.deref(receiver.clone());
        if let Ok((array_addr, Value::Array(arr))) = dereferenced {
            // TODO check arity
            match (str_name, args[0].clone(), args.get(1)) {
                ("get", Value::Int(i), _) => {
                    (i >= 0 && (i as usize) < arr.len()).expect(|| anyhow!("array index {} out of bounds", i))?;
                    let v = self.get(self.array_get(array_addr as usize, i as u32)? as usize)?;
                    self.push(v)?;
                },
                ("set", v, Some(&Value::Int(i))) => {
                    (i >= 0 && (i as usize) < arr.len()).expect(|| anyhow!("array index {} out of bounds", i))?;
                    let addr = self.alloc(&v)?;
                    self.array_set(array_addr as usize, i as u32, addr as u64)?;
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
        } else if let Ok(
            (receiver_addr, Value::Object { parent, methods, fields: _ })
        ) = dereferenced {
            match methods.get(&name) {
                Some(&i) => {
                    let (_name, n_args, n_locals, start, len) = self.constant_pool[i as usize].as_method()?;
                    (n_args as usize == args.len() + 1).expect(|| anyhow!(
                        "wrong number of arguments, {} expects {}, not {} (receiver and {:?})", name, n_args, args.len() + 1, args
                    ))?;
                    let mut allocated_args = Vec::with_capacity(n_args as usize + 1);
                    for arg in args {
                        allocated_args.push(self.alloc(arg)?);
                    }
                    allocated_args.push(receiver_addr as usize);
                    self.call_function(start, start + len, n_locals, &allocated_args)
                },
                None => self.call_method(self.get(parent as usize)?, name, args),
            }
        } else {
            Err(anyhow!("cannot call method on {}", self.show(&receiver)))
        }
    }

    fn show(&self, v: &Value) -> String {
        fn go(this: &Interpreter, depth: u8, v: &Value) -> String {
            if depth > 10 {
                return "[deep!]".to_string();
            }

            match v {
                Value::Int(n) => n.to_string(),
                Value::Bool(b) => b.to_string(),
                &Value::Reference(addr) => go(this, depth + 1, &this.get(addr as usize).unwrap()),
                Value::Array(arr) => format!("[{}]", arr
                    .iter()
                    .map(|&addr| go(this, depth + 1, &this.get(addr as usize).unwrap()))
                    .collect_vec()
                    .join(", ")
                ),
                Value::Object { parent, fields, methods: _ } => format!(
                    "object({})",
                    (match this.get(*parent as usize).unwrap() {
                        Value::Null => None,
                        obj => Some(format!("..={}", go(this, depth + 1, &obj))),
                    }).into_iter().chain(fields.iter()
                        .sorted_by_key(|f| this.constant_pool[f.0 as usize].as_string().unwrap())
                        .map(|(k, v)| format!(
                        "{}={}",
                        this.constant_pool[*k as usize].as_string().unwrap(),
                        go(this, depth + 1, &this.get(*v as usize).unwrap())
                    )))
                    .collect_vec()
                    .join(", ")
                ),
                Value::Null => "null".to_string(),
            }
        }

        go(self, 0, v)
    }

    fn call_builtin_method(&self, receiver: Value, name: &str, arg: Value) -> Result<Value> {
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

    fn call_function(&mut self, start: Pc, end: Pc, n_locals: u16, args: &[usize]) -> Result<()> {
        let null_ptr = self.alloc(&Value::Null)?;
        self.call_stack.push(StackFrame {
            code_end: end,
            return_address: self.pc + 1,
            locals: args.iter().rev().chain((0..n_locals).map(|_| &null_ptr)).copied().collect(),
        });
        self.pc = start;
        Ok(())
    }

    fn addr_of_local(&mut self, i: u16) -> Result<&mut usize> {
        self.frame().locals.get_mut(i as usize).ok_or_else(|| anyhow!("index {} out of range", i))
    }

    fn get_field(&mut self, obj: &Value, name: u16) -> Result<&mut [u8/*; 8*/]> {
        use Value::*;
        let str_name = self.constant_pool[name as usize].as_str().unwrap();
        // FIXME avoid cloning
        match self.deref(obj.clone())? {
            (addr, Object { parent: _, fields, methods: _ }) => {
                let index = fields.binary_search_by_key(&name, |p| p.0)
                    // TODO tail-recurse for parent
                    .map_err(|_| anyhow!("no such field {}", str_name))?;

                let field_value_addr = addr as usize
                    + 1  // tag
                    + 24 // parent, field and method counts
                    + index as usize * 10 // field index
                    + 2; // skip key
                Ok(self.heap[field_value_addr .. field_value_addr + 8].as_mut())
            },
            _ => Err(anyhow!(
                "cannot access field {} of {}. In fact, it doesn't even matter
                what the field's name is, as {} is not an object.", str_name, self.show(obj), self.show(obj)
            )),
        }
    }

    fn print(&mut self, format: &str, args: &[Value]) -> Result<Value> {
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
}

fn run(this: &mut Interpreter) -> Result<()> {
    while !this.call_stack.is_empty() && this.pc < this.call_stack.last().unwrap().code_end {
        #[cfg(debug_assertions)]
        {
            println!("\tstack: {:?}", this.stack);
            print!("pc: {pc}, globals: ", pc = this.pc);
            for (name, target) in this.global_map.iter() {
                if let Left(addr) = target {
                    print!("{}.{}={} ", addr, name, this.show(&this.get(*addr)?));
                }
            }
            print!(" locals: ");
            for (i, addr) in this.frame().locals.clone().into_iter().enumerate() {
                print!("[{}]={} ", i, this.show(&this.get(addr)?));
            }
            println!("i: {instr:?}", instr = this.code[this.pc]);
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
            Instr::Print(idx, n_args) => {
                let args = (0..n_args).map(|_| this.pop()).collect::<Result<Vec<_>>>()?;
                let format_string = this.constant_pool[idx as usize].as_string()?;
                this.print(&format_string, &args.into_iter().rev().collect_vec())?;
                this.push(Value::Null)?;
                Ok(())
            },
            Instr::GetLocal(i) => {
                let addr = *this.addr_of_local(i)?;
                this.push(match this.get(addr)? {
                    Value::Array(_) | Value::Object { .. } => Value::Reference(addr as u64),
                    v => v,
                })?;
                Ok(())
            },
            Instr::SetLocal(i) => {
                let v = this.peek()?;
                *this.addr_of_local(i)? = this.alloc(&v)?;
                Ok(())
            },
            Instr::GetGlobal(i) => {
                // replace with a direct dereference
                increment = false;
                // let name = this.constant_pool[i as usize].as_string()?;
                this.code[this.pc] = Instr::GetGlobalDirect(i);
                Ok(())
            },
            Instr::GetGlobalDirect(name) => {
                this.push(this.get(this.global_map[&name].unwrap_left())?)?;
                Ok(())
            },
            Instr::SetGlobal(i) => {
                // replace with a direct set
                increment = false;
                // let name = this.constant_pool[i as usize].as_string()?;
                this.code[this.pc] = Instr::SetGlobalDirect(i);
                Ok(())
            },
            Instr::SetGlobalDirect(name) => {
                let v = this.peek()?;
                let addr = this.alloc(&v)?;
                this.global_map.insert(name, Left(addr));
                Ok(())
            },
            Instr::GetField(i) => {
                // replace with a direct get
                increment = false;
                // let name = this.constant_pool[i as usize].as_string()?;
                this.code[this.pc] = Instr::GetFieldDirect(i);
                Ok(())
            },
            Instr::GetFieldDirect(name) => {
                let obj = this.pop()?;
                let ref_bytes = this.get_field(&obj, name)?;
                let addr = u64::from_le_bytes(ref_bytes.try_into().unwrap()) as usize;
                this.push(this.get(addr)?)?;
                Ok(())
            },
            Instr::SetField(i) => {
                // replace with a direct set
                increment = false;
                // let name = this.constant_pool[i as usize].as_string()?;
                this.code[this.pc] = Instr::SetFieldDirect(i);
                Ok(())
            },
            Instr::SetFieldDirect(name) => {
                let v = this.pop()?;
                let obj = this.pop()?;
                let v_addr = this.alloc(&v)?;
                let ref_bytes = this.get_field(&obj, name)?;
                ref_bytes.copy_from_slice(v_addr.to_le_bytes().as_slice());
                this.push(v)?;
                Ok(())
            },
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
                let args = (0..n_args - 1).map(|_| this.pop()).collect::<Result<Vec<_>>>()?;
                let receiver = this.pop()?;
                this.call_method(receiver, i, &args)?;
                Ok(())
            },
            Instr::CallFunction(i, n_args) => {
                increment = false;
                let global_name = this.constant_pool[i as usize].as_str()?;
                let fn_addr = this.global_map.get(&i)
                    .ok_or_else(|| anyhow!("compiler bug: global {} not found", global_name))?
                    .right()
                    .ok_or_else(|| anyhow!("attempt to call variable {}", global_name))?;

                let (_, arity, n_locals, start, len) = this.constant_pool[fn_addr as usize].as_method()?;
                (arity == n_args).expect(||
                    anyhow!("arity mismatch, expected {} arguments, got {}", arity, n_args)
                )?;

                let args = (0..n_args)
                    .map(|_| this.pop().and_then(|arg| this.alloc(&arg)))
                    .collect::<Result<Vec<_>>>()?;

                this.call_function(start, start + len, n_locals, &args)?;
                Ok(())
            },
            Instr::Return => {
                increment = false;
                this.perform_return()
            },
            Instr::Array => {
                let initial = this.pop()?;
                let len = this.pop()?.as_int(
                    |v| anyhow!("the length of an array must be an integer (not {})", this.show(v))
                )?;

                (len >= 0).expect(|| anyhow!("the length of an array must be non-negative"))?;
                let len = len as usize;
                let el_addr = this.alloc(&initial)? as u64;
                let arr_addr = this.alloc(&Value::Array(vec![el_addr; len]))?;

                this.push(Value::Reference(arr_addr as u64))
            },
            Instr::Object(class) => {
                // FIXME do this all in a single loop
                let mut members = this.constant_pool[class as usize].as_class()?
                    .into_iter()
                    .map(|i| (i, this.constant_pool[i as usize].clone()))
                    .map(|(i, k)| match k {
                        Constant::Method { .. } => Ok((i as i16, k)),
                        Constant::Slot(_) => Ok((-1, k)),
                        _ => Err(anyhow!("an object cannot contain a {:?}", k)),
                    })
                    .collect::<Result<Vec<_>>>()?;

                members.sort_by_key(|p| p.0);
                members.reverse();

                let mut fields = vec![];
                let mut methods = vec![];
                for (method_index, slot) in members.into_iter() {
                    match slot {
                        Constant::Slot(i) => {
                            let v = this.pop()?;
                            let addr = this.alloc(&v)?;
                            fields.push((i, addr as u64));
                        },
                        Constant::Method { name_idx, .. } => {
                            methods.push((name_idx, method_index as u16))
                        },
                        _ => unreachable!(),
                    }
                }

                fields.sort_by_key(|p| p.0);

                // FIXME this is one of the places with the stack value / heap object distinction
                let parent = this.pop()?;
                let parent = this.alloc(&parent)? as u64;
                let obj = Value::Object {
                    parent,
                    fields,
                    methods: methods.into_iter().collect(),
                };

                let addr = this.alloc(&obj)? as u64;
                this.push(Value::Reference(addr))
            },
        }?;

        this.pc += increment as usize;
    };
    Ok(())
    // Err(anyhow!("nope"))
}

fn main() -> Result<()> {
    let mut i = Interpreter::load(&mut std::io::stdin())?;
    match i.execute() {
        Ok(()) => Ok(()),
        Err(e) => {
            let Interpreter {
                call_stack,
                code,
                constant_pool,
                global_map,
                label_map,
                pc,
                stack,
                heap: _,
            } = i;
            eprintln!(
                "interpreter crash:\n\
                call_stack = {call_stack:#?}\n\
                code = {code:?}\n\
                constant_pool = {constant_pool:?}\n\
                global_map = {global_map:#?}\n\
                label_map = {label_map:#?}\n\
                pc = {pc:#?}\n\
                stack = {stack:#?}\n\
                ",
                call_stack = call_stack,
                code = code,
                constant_pool = constant_pool,
                global_map = global_map,
                label_map = label_map,
                pc = pc,
                stack = stack,
            );
            Err(e)
        },
    }
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
        Interpreter::load(&mut buf).unwrap()
    }

    #[test]
    fn arrays() -> Result<()> {
        use Value::*;

        let mut i = dummy();
        let null = i.alloc(&Null)? as u64;
        let arr = i.alloc(&Array(vec![null; 10]))?;

        let int1 = i.alloc(&Int(1))? as u64;
        let int2 = i.alloc(&Int(-2))? as u64;
        let int3 = i.alloc(&Int(3))? as u64;
        let int4 = i.alloc(&Int(4))? as u64;

        i.array_set(arr, 1, int2)?;
        i.array_set(arr, 0, int1)?;
        i.array_set(arr, 2, int3)?;
        i.array_set(arr, 9, int4)?;

        assert_eq!(i.get(i.array_get(arr, 0)? as usize)?, Int(1));
        assert_eq!(i.get(i.array_get(arr, 1)? as usize)?, Int(-2));
        assert_eq!(i.get(i.array_get(arr, 2)? as usize)?, Int(3));
        assert_eq!(i.get(i.array_get(arr, 9)? as usize)?, Int(4));

        Ok(())
    }
}
