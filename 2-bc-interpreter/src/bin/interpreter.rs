#![feature(fn_traits, associated_type_defaults)]

use std::{collections::BTreeMap, fmt::Display};
use fml_bc_interpreter::util::BooleanAssertions;
use itertools::Itertools;
use anyhow::Result;
use anyhow::anyhow;

use fml_bc_interpreter::*;
use bc::*;

#[derive(Debug, Clone)]
enum Value {
    Int(i32),
    Bool(bool),
    Null,
}

impl From<&Value> for Vec<u8> {
    fn from(value: &Value) -> Self {
        let (tag, mut repr) = match value {
            Value::Int(i) => (1, i.to_le_bytes().to_vec()),
            Value::Bool(b) => (2, vec![if *b { 1 } else { 0 }]),
            Value::Null => (4, vec![]),
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
            4 => Value::Null,
            _ => panic!("Invalid value tag"),
        }
    }
}

impl Value {
    fn as_bool(&self, err: fn(&Value) -> anyhow::Error) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            v => Err(err(v)),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Null)
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Value::Int(i) => write!(fmt, "{}", i),
            Value::Bool(b) => write!(fmt, "{}", b),
            Value::Null => write!(fmt, "null"),
        }
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
    global_map: BTreeMap<String, usize>,
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
        let (constant_pool, code) = bc::parse_constant_pool(&mut iter)?;
        let globals = bc::parse_globals(&mut iter)?;
        let entry_point = bc::parse_entry_point(&mut iter)?;
        let (code, label_map) = bc::collect_labels(&constant_pool[..], code)?;

        let (pc, init_locals) = match &constant_pool[entry_point as usize] {
            Constant::Method { name_idx: _, n_args: _, n_locals, start, length: _ } => Ok((*start, *n_locals)),
            _ => Err(anyhow!("Invalid entry point")),
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
            locals: (0..init_locals).map(|_| init.alloc(&Value::Null)).collect::<Result<_>>()?,
            return_address: 0,
        };

        init.call_stack.push(bottom_frame);
        init.init_with_globals(globals)
    }

    fn init_with_globals(mut self, globals: Vec<u16>) -> Result<Interpreter> {
        let safe_string = |i| -> Result<String> {
            self.constant_pool.get(i as usize)
                .ok_or_else(|| anyhow!("invalid global, {} is outside the constant pool", i))
                .and_then(Constant::as_string)
        };

        let global_names = globals.iter()
            .map(|&i| self.constant_pool.get(i as usize)
                .ok_or_else(|| anyhow!("invalid global, {} is outside the constant pool", i))
                .and_then(|k| match *k {
                    Constant::Slot(i) => Ok(safe_string(i)?),
                    Constant::Method { name_idx, .. } => Ok(safe_string(name_idx)?),
                    _ => Err(anyhow!("Invalid global {:?}", k)),
                })
            )
            .collect::<Result<Vec<_>>>()?;

        let mut global_map = BTreeMap::new();
        for name in global_names {
            let addr = self.alloc(&Value::Null)?;
            global_map.insert(name, addr);
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

    // fn top(&mut self, statements: &[AST]) -> Result<Value> {
    //     statements.iter().fold(Ok(Value::Null), |acc, s| acc.and(self.eval(s)))
    // }

    // fn block(&mut self, statements: &[AST]) -> Result<Value> {
    //     self.top(statements)
    // }

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
        if self.pc >= self.code.len() {
            return Ok(Value::Null)
            // return Err(anyhow!("program counter out of bounds"))
        }

        #[cfg(debug_assertions)]
        {
            print!("pc: {pc}, globals: ", pc = self.pc);
            for (name, addr) in self.global_map.iter() {
                print!("{}.{}={} ", addr, name, self.get(*addr)?);
            }
            println!("i: {instr:?}", instr = self.code[self.pc]);
            println!("\tstack: {:?}", self.stack);
        }

        let mut increment = true;
        match self.code[self.pc].clone() {
            Instr::Literal(i) => {
                increment = false;
                self.code[self.pc] = match self.constant_pool[i as usize] {
                    Constant::Null => Ok(Instr::LiteralNull),
                    Constant::Boolean(b) => Ok(Instr::LiteralBool(b)),
                    Constant::Integer(n) => Ok(Instr::LiteralInt(n)),
                    Constant::String(_) => Err(anyhow!("attempt to push a string onto the stack")),
                    Constant::Slot(_) => Err(anyhow!("attempt to push a slot onto the stack")),
                    Constant::Method { .. } => Err(anyhow!("attempt to push a method onto the stack")),
                }?;
                Ok(()) as Result<()>
            },
            Instr::LiteralNull => {
                self.push(Value::Null)?;
                Ok(())
            },
            Instr::LiteralBool(b) => {
                self.push(Value::Bool(b))?;
                Ok(())
            },
            Instr::LiteralInt(n) => {
                self.push(Value::Int(n))?;
                Ok(())
            },
            Instr::Drop => {
                self.pop()?;
                Ok(())
            },
            Instr::Print(idx, n_args) => {
                let args = (0..n_args).map(|_| self.pop()).collect::<Result<Vec<_>>>()?;
                let format_string = self.constant_pool[idx as usize].as_string()?;
                self.print(&format_string, &args.into_iter().rev().collect_vec())?;
                self.push(Value::Null)?;
                Ok(())
            },
            Instr::GetLocal(i) => {
                let addr = *self.addr_of_local(i)?;
                self.push(self.get(addr)?)?;
                Ok(())
            },
            Instr::SetLocal(i) => {
                let v = self.peek()?;
                *self.addr_of_local(i)? = self.alloc(&v)?;
                Ok(())
            },
            Instr::GetGlobal(i) => {
                // replace with a direct dereference
                increment = false;
                let name = self.constant_pool[i as usize].as_string()?;
                self.code[self.pc] = Instr::GetGlobalDirect(name);
                Ok(())
            },
            Instr::GetGlobalDirect(name) => {
                self.push(self.get(self.global_map[&name] as usize)?)?;
                Ok(())
            },
            Instr::SetGlobal(i) => {
                // replace with a direct set
                increment = false;
                let name = self.constant_pool[i as usize].as_string()?;
                self.code[self.pc] = Instr::SetGlobalDirect(name);
                Ok(())
            },
            Instr::SetGlobalDirect(name) => {
                let v = self.peek()?;
                let addr = self.alloc(&v)?;
                self.global_map.insert(name, addr);
                Ok(())
            },
            Instr::Label(_) => unreachable!("labels should be removed by the label collection pass"),
            Instr::Jump(i) => {
                // replace with a direct jump
                increment = false;
                let label = self.constant_pool[i as usize].as_string()?;
                let addr = self.label_map[&label];
                self.code[self.pc] = Instr::JumpDirect(addr);
                Ok(())
            },
            Instr::JumpDirect(target) => {
                increment = false;
                self.pc = target;
                Ok(())
            },
            Instr::Branch(i) => {
                // replace with a direct branch
                increment = false;
                let label = self.constant_pool[i as usize].as_string()?;
                let addr = self.label_map[&label];
                self.code[self.pc] = Instr::BranchDirect(addr);
                Ok(())
            },
            Instr::BranchDirect(target) => {
                let v = self.pop()?;
                if v.is_truthy() {
                    increment = false;
                    self.pc = target;
                }
                Ok(())
            },
            Instr::CallFunction(i, n_args) => {
                let global_name = self.constant_pool[i as usize].as_string()?;
                let fn_addr = self.global_map[&global_name];
                let (_, arity, n_locals, start, _) = self.constant_pool[fn_addr].as_method()?;
                (arity == n_args).expect(||
                    anyhow!("arity mismatch, expected {} arguments, got {}", arity, n_args)
                )?;

                let args = (0..n_args)
                    .map(|_| self.pop().and_then(|arg| self.alloc(&arg)))
                    .collect::<Result<Vec<_>>>()?;

                self.call_function(start, n_locals, &args)?;
                Ok(())
            },
            Instr::Return => {
                let StackFrame {
                    return_address,
                    locals: _,
                } = self.call_stack.pop().unwrap();
                (!self.call_stack.is_empty()).expect(||
                    anyhow!("popped the global frame")
                )?;

                self.pc = return_address;
                Ok(())
            },
        }?;

        self.pc += increment as usize;
        self.execute()
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

    // fn variable(&mut self, name: &Identifier, value: &AST) -> Result<Value> {
    //     let v = self.eval(value)?;
    //     let addr = self.alloc(&v)?;
    //     self.env.insert(name.clone(), addr);
    //     Ok(v)
    // }

    // fn assign_variable(&mut self, name: &Identifier, value: &AST) -> Result<()> {
    //     let v = self.eval(value)?;
    //     match self.env.get(name) {
    //         Some(addr) => {
    //             self.set(*addr, &v)?;
    //             Ok(())
    //         },
    //         None => Err(anyhow!(
    //             "you don't seem to understand how variable assignments work. {} is not defined.", name.0
    //         )),
    //     }
    // }

    // fn call_function(&mut self, name: &Identifier, arguments: &[AST]) -> Result<Value> {
    //     match self.lookup(name)?.clone() {
    //         Value::Function { parameters, .. } if parameters.len() != arguments.len() =>
    //             Err(anyhow!(
    //                 "it helps to learn how to count before writing a function like {}. It takes {} arguments, not {}.",
    //                 name.0,
    //                 parameters.len(),
    //                 arguments.len()
    //             )),
    //         Value::Function { parameters, body } => {
    //             let env = self.env.clone();
    //             parameters
    //                 .iter()
    //                 .zip(arguments.iter())
    //                 .map(|(n, expr)| self.eval(expr).map(|v| (n, v)))
    //                 .collect::<Result<Vec<_>>>()?
    //                 .into_iter()
    //                 .try_for_each::<_, Result<()>>(|(name, v)| {
    //                     let addr = self.alloc(&v)?;
    //                     self.bind(name, addr)?;
    //                     Ok(())
    //                 })?;

    //             let result = self.eval(&*body)?;
    //             self.env = env;
    //             Ok(result)
    //         },
    //         v => Err(anyhow!(
    //             "you tried to call {}, but it didn't work. Maybe you didn't try hard enough, or \
    //             maybe the stars aren't aligned right today, or maybe it's the fact that {} holds \
    //             a value of {}, not a function. Who knows?", name.0, name.0, v
    //         )),
    //     }
    // }

    // fn loop_de_loop(&mut self, condition: &AST, body: &AST) -> Result<()> {
    //     while (self.eval(condition)?).as_bool(|v| anyhow!(
    //         "the difference between your program and most others \
    //         is that other people tend to put booleans as their loop conditions. \
    //         Using a value of {} instead may just become the next big thing. Keep at it.", v
    //     ))? {
    //         self.eval(body)?;
    //     }
    //     Ok(())
    // }

    // fn conditional(&mut self, condition: &AST, consequent: &AST, alternative: &AST) -> Result<Value> {
    //     match self.eval(condition)? {
    //         Value::Bool(true) => self.eval(consequent),
    //         Value::Bool(false) => self.eval(alternative),
    //         v => Err(anyhow!("you're trying to branch on a {}. Do you really think that's a good idea?", v)),
    //     }
    // }

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
                        str.push_str(&arg.to_string());
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

fn main() -> Result<()> {
    let mut i = Interpreter::load(&mut std::io::stdin())?;
    match i.execute() {
        Ok(_) => Ok(()),
        Err(e) => {
            eprintln!("interpreter crash:\n{:#?}", i);
            Err(e)
        }
    }
}
