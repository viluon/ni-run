#![feature(fn_traits, associated_type_defaults)]

use std::{collections::BTreeMap, fmt::Display};
use fml_bc_interpreter::util::BooleanAssertions;
use itertools::Itertools;
use itertools::Either;
use itertools::Either::{Left, Right};
use anyhow::Result;
use anyhow::anyhow;
use tailcall::tailcall;

use fml_bc_interpreter::*;
use bc::*;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Value {
    Int(i32),
    Bool(bool),
    Reference(u64),
    Array(Vec<u64>), // TODO: copy on write?
    Object { parent: u64, fields: Vec<(String, u64)>, methods: BTreeMap<String, u16> },
    Null,
}

impl From<&Value> for Vec<u8> {
    fn from(value: &Value) -> Self {
        let (tag, mut repr) = match value {
            Value::Int(i) => (1, i.to_le_bytes().to_vec()),
            Value::Bool(b) => (2, vec![if *b { 1 } else { 0 }]),
            Value::Array(v) => (3, vec![
                (v.len() as u64).to_le_bytes().to_vec(),
                v.iter().flat_map(|x| x.to_le_bytes().into_iter()).collect()
            ].concat()),
            Value::Null => (4, vec![]),
            Value::Reference(addr) => (5, addr.to_le_bytes().to_vec()),
            Value::Object { parent, fields, methods } => (6, {
                vec![
                    (parent.to_le_bytes().to_vec()),
                    (fields.len() as u64).to_le_bytes().to_vec(),
                    (methods.len() as u64).to_le_bytes().to_vec(),
                    fields.iter().flat_map(|(k, v)| {
                        k.len().to_le_bytes().iter().copied()
                            .chain(k.as_bytes().iter().copied()
                                .chain(v.to_le_bytes().iter().copied())
                            )
                        .collect_vec()
                    }).collect_vec(),
                    methods.iter().flat_map(|(k, v)| {
                        k.len().to_le_bytes().iter().copied()
                            .chain(k.as_bytes().iter().copied()
                                .chain(v.to_le_bytes().iter().copied())
                            )
                        .collect_vec()
                    }).collect_vec(),
                ].concat()
            }),
        };
        repr.insert(0, tag);
        repr
    }
}

impl From<&[u8]> for Value {
    fn from(bytes: &[u8]) -> Self {
        let tag = bytes[0];
        let bytes = &bytes[1..];
        match tag {
            1 => Value::Int(i32::from_le_bytes(bytes[..4].try_into().unwrap())),
            2 => Value::Bool(bytes[0] == 1),
            3 => Value::Array({
                let len = u64::from_le_bytes(bytes[..8].try_into().unwrap());
                let mut v = Vec::with_capacity(len as usize);
                for i in 1..=len as usize {
                    v.push(u64::from_le_bytes(bytes[(i * 8)..((i + 1) * 8)].try_into().unwrap()));
                }
                v
            }),
            4 => Value::Null,
            5 => Value::Reference(u64::from_le_bytes(bytes[..8].try_into().unwrap())),
            6 => {
                let parent = u64::from_le_bytes(bytes[..8].try_into().unwrap());
                let len_fields = u64::from_le_bytes(bytes[8..16].try_into().unwrap()) as usize;
                let len_methods = u64::from_le_bytes(bytes[16..24].try_into().unwrap()) as usize;
                let mut bytes = &bytes[24..];
                let mut fields = vec![];
                let mut methods = BTreeMap::new();
                for _ in 0..len_fields {
                    let len = u64::from_le_bytes(bytes[..8].try_into().unwrap()) as usize;
                    let key = String::from_utf8(bytes[8..8 + len].try_into().unwrap()).unwrap();
                    let value = u64::from_le_bytes(bytes[8 + len..16 + len].try_into().unwrap());
                    fields.push((key, value));
                    bytes = &bytes[16 + len..];
                }
                for _ in 0..len_methods {
                    let len = u64::from_le_bytes(bytes[..8].try_into().unwrap()) as usize;
                    let key = String::from_utf8(bytes[8..8 + len].try_into().unwrap()).unwrap();
                    let value = u16::from_le_bytes(bytes[8 + len..10 + len].try_into().unwrap());
                    methods.insert(key, value);
                    bytes = &bytes[10 + len..];
                }
                Value::Object { parent, fields, methods }
            },
            _ => panic!("Invalid value tag"),
        }
    }
}

impl Value {
    fn as_bool<F: FnOnce(&Value) -> anyhow::Error>(&self, err: F) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            v => Err(err(v)),
        }
    }

    fn as_int<F: FnOnce(&Value) -> anyhow::Error>(&self, err: F) -> Result<i32> {
        match self {
            Value::Int(i) => Ok(*i),
            v => Err(err(v)),
        }
    }

    fn as_array<F: FnOnce(&Value) -> anyhow::Error>(&self, err: F) -> Result<&Vec<u64>> {
        match self {
            Value::Array(v) => Ok(v),
            v => Err(err(v)),
        }
    }

    fn as_reference<F: FnOnce(&Value) -> anyhow::Error>(&self, err: F) -> Result<u64> {
        match self {
            Value::Reference(addr) => Ok(*addr),
            v => Err(err(v)),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Null)
    }
}

const STACK_LIMIT: usize = 1024;

#[derive(Debug, Clone, PartialEq, Eq)]
struct StackFrame {
    locals: Vec<usize>,
    return_address: Pc,
}

#[derive(Debug, Default, Clone)]
struct Interpreter {
    constant_pool: Vec<Constant>,
    label_map: BTreeMap<String, Pc>,
    // the keys are both heap addresses and constant pool indices,
    // depending on whether the global is a variable or a function
    global_map: BTreeMap<String, Either<usize, u16>>,
    // globals vector from bytecode
    globals: Vec<u16>,
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

        let (pc, init_locals) = match &constant_pool[entry_point] {
            Constant::Method { name_idx: _, n_args: _, n_locals, start, length: _ } => Ok((*start, *n_locals)),
            _ => Err(anyhow!("Invalid entry point")),
        }?;

        let mut init = Interpreter {
            stack: vec![],
            call_stack: vec![],
            heap: vec![],
            global_map: Default::default(),
            globals: vec![],
            constant_pool,
            label_map,
            code,
            pc,
        };

        let bottom_frame = StackFrame {
            locals: (0..init_locals).map(|_| init.alloc(&Value::Null)).collect::<Result<_>>()?,
            return_address: 0,
        };

        init.call_stack.push(bottom_frame);
        init.init_with_globals(globals)
    }

    fn init_with_globals(mut self, globals: Vec<u16>) -> Result<Interpreter> {
        let mut global_map = BTreeMap::new();
        for &k_idx in globals.iter() {
            self.constant_pool.get(k_idx as usize)
                .ok_or_else(|| anyhow!("invalid global, {} is outside the constant pool", k_idx))
                .cloned()
                .and_then(|k| match k {
                    Constant::Slot(i) => {
                        let name = self.constant_pool.get(i as usize)
                            .ok_or_else(|| anyhow!("invalid global, {} is outside the constant pool", i))
                            .and_then(Constant::as_string)?;
                        global_map.insert(name, Left(self.alloc(&Value::Null)?));
                        Ok(())
                    },
                    Constant::Method { name_idx, .. } => self.constant_pool[name_idx as usize]
                        .as_string()
                        .map(|name| {
                            global_map.insert(name, Right(k_idx));
                        }),
                    _ => Err(anyhow!("Invalid global {:?}", k)),
                })?;
        }

        Ok(Interpreter { global_map, globals, ..self })
    }

    fn alloc(&mut self, v: &Value) -> Result<usize> {
        let addr = self.heap.len();
        self.set(addr, v)?;
        Ok(addr)
    }

    fn get(&self, addr: usize) -> Result<Value> {
        if addr < self.heap.len() {
            Ok(Value::from(&self.heap[addr..]))
        } else { Err(anyhow!("invalid address: {:04x}", addr)) }
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

    fn execute(&mut self) -> Result<Value> {
        run(self)
    }

    fn array_index(&self, arr: usize, i: u32) -> Result<usize> {
        let i = i as usize;
        let v = self.get(arr)?;
        let arr = v.as_array(|v| anyhow!("expected an array, got {}", self.show(&v)))?;
        (i < arr.len()).expect(|| anyhow!("array index out of bounds"))?;
        Ok(arr[i] as usize)
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

    fn deref(&self, mut v: Value) -> Result<Option<(u64, Value)>> {
        let mut array_addr = None;
        while let Value::Reference(addr) = v {
            array_addr = Some(addr);
            v = self.get(addr as usize)?
        }
        Ok(Some((array_addr.unwrap(), v)))
    }

    fn call_method(&mut self, receiver: Value, name: String, args: &[Value]) -> Result<()> {
        if matches!(receiver, Value::Array(_) | Value::Int(_) | Value::Bool(_) | Value::Null) {
            // TODO check arity
            self.push(self.call_builtin_method(receiver, name.as_str(), args[0].clone())?)?;
            return Ok(());
        } else if let Some((array_addr, Value::Array(arr))) = self.deref(receiver)? {
            // TODO check arity
            match (name.as_str(), args[0].clone()) {
                ("get", Value::Int(i)) => {
                    (i >= 0 && (i as usize) < arr.len()).expect(|| anyhow!("array index {} out of bounds", i))?;
                    let v = self.get(self.array_get(array_addr as usize, i as u32)? as usize)?;
                    self.push(v)?;
                },
                ("set", Value::Int(i)) => {
                    (i >= 0 && (i as usize) < arr.len()).expect(|| anyhow!("array index {} out of bounds", i))?;
                    let v = args[1].clone();
                    let addr = self.alloc(&v)?;
                    self.array_set(array_addr as usize, i as u32, addr as u64)?;
                    self.push(v)?;
                },
                ("get" | "set", v) => return Err(anyhow!(
                    "indexing an array with a {} might work in PHP, but it won't work here.", self.show(&v)
                )),
                (invalid, _) => return Err(anyhow!(
                    "this is actually the very first time someone thought to call {} on an array.", invalid
                )),
            }
            return Ok(());
        }

        todo!()
    }

    fn show(&self, v: &Value) -> String {
        match v {
            Value::Int(n) => n.to_string(),
            Value::Bool(b) => b.to_string(),
            &Value::Reference(addr) => self.show(&self.get(addr as usize).unwrap()),
            Value::Array(arr) => format!("[{}]", arr
                .iter()
                .map(|&addr| self.show(&self.get(addr as usize).unwrap()))
                .collect_vec()
                .join(", ")
            ),
            Value::Object { parent, fields, methods } => format!(
                "object({})",
                (match self.get(*parent as usize).unwrap() {
                    Value::Null => None,
                    obj => Some(format!("..={}", self.show(&obj))),
                }).into_iter().chain(fields
                    .iter()
                    .map(|(k, v)| format!("{}={}", k, self.show(&self.get(*v as usize).unwrap())))
                )
                .collect_vec()
                .join(", ")
            ),
            Value::Null => "null".to_string(),
        }
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

    fn call_function(&mut self, start: Pc, n_locals: u16, args: &[usize]) -> Result<()> {
        let null_ptr = self.alloc(&Value::Null)?;
        self.call_stack.push(StackFrame {
            return_address: self.pc + 1,
            locals: args.iter().rev().chain((0..n_locals).map(|_| &null_ptr)).copied().collect(),
        });
        self.pc = start;
        Ok(())
    }

    fn addr_of_local(&mut self, i: u16) -> Result<&mut usize> {
        self.frame().locals.get_mut(i as usize).ok_or_else(|| anyhow!("index {} out of range", i))
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
                        str.push_str(&self.show(&arg));
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

fn run(this: &mut Interpreter) -> Result<Value> {
    loop {
        if this.pc >= this.code.len() {
            return Ok(Value::Null)
            // return Err(anyhow!("program counter out of bounds"))
        }

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
                this.push(this.get(addr)?)?;
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
                let name = this.constant_pool[i as usize].as_string()?;
                this.code[this.pc] = Instr::GetGlobalDirect(name);
                Ok(())
            },
            Instr::GetGlobalDirect(name) => {
                this.push(this.get(this.global_map[&name].unwrap_left())?)?;
                Ok(())
            },
            Instr::SetGlobal(i) => {
                // replace with a direct set
                increment = false;
                let name = this.constant_pool[i as usize].as_string()?;
                this.code[this.pc] = Instr::SetGlobalDirect(name);
                Ok(())
            },
            Instr::SetGlobalDirect(name) => {
                let v = this.peek()?;
                let addr = this.alloc(&v)?;
                this.global_map.insert(name, Left(addr));
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
                // TODO increment = false?
                let name = this.constant_pool[i as usize].as_string()?;
                let args = (0..n_args - 1).map(|_| this.pop()).collect::<Result<Vec<_>>>()?;
                let receiver = this.pop()?;
                this.call_method(receiver, name, &args.into_iter().rev().collect_vec())?;
                Ok(())
            },
            Instr::CallFunction(i, n_args) => {
                increment = false;
                let global_name = this.constant_pool[i as usize].as_string()?;
                let fn_addr = this.global_map[&global_name].right()
                    .ok_or_else(|| anyhow!("attempt to call variable {}", global_name))?;

                let (_, arity, n_locals, start, _) = this.constant_pool[fn_addr as usize].as_method()?;
                (arity == n_args).expect(||
                    anyhow!("arity mismatch, expected {} arguments, got {}", arity, n_args)
                )?;

                let args = (0..n_args)
                    .map(|_| this.pop().and_then(|arg| this.alloc(&arg)))
                    .collect::<Result<Vec<_>>>()?;

                this.call_function(start, n_locals, &args)?;
                Ok(())
            },
            Instr::Return => {
                increment = false;
                let StackFrame {
                    return_address,
                    locals: _,
                } = this.call_stack.pop().unwrap();
                (!this.call_stack.is_empty()).expect(||
                    anyhow!("popped the global frame")
                )?;

                this.pc = return_address;
                Ok(())
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
                            let name = this.constant_pool[i as usize].as_string()?;
                            let v = this.pop()?;
                            let addr = this.alloc(&v)?;
                            fields.push((name, addr as u64));
                        },
                        Constant::Method { name_idx, .. } => {
                            let name = this.constant_pool[name_idx as usize].as_string()?;
                            methods.push((name, method_index as u16))
                        },
                        _ => unreachable!(),
                    }
                }

                fields.reverse();

                // FIXME this is one of the places with the stack value / heap object distinction
                let parent = this.pop()?;
                let parent = this.alloc(&parent)? as u64;

                this.push(Value::Object {
                    parent,
                    fields,
                    methods: methods.into_iter().collect(),
                })
            },
        }?;

        this.pc += increment as usize;
    }
}

fn main() -> Result<()> {
    let mut i = Interpreter::load(&mut std::io::stdin())?;
    match i.execute() {
        Ok(_) => Ok(()),
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
                globals: _,
            } = i;
            eprintln!(
                "interpreter crash:\n\
                call_stack = {call_stack:#?}\n\
                code = {code:#?}\n\
                constant_pool = {constant_pool:#?}\n\
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
