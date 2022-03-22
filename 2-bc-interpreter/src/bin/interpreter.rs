#![feature(fn_traits, associated_type_defaults)]

use std::{io::Read, collections::BTreeMap, fmt::Display};
use itertools::Itertools;
use anyhow::Result;
use anyhow::anyhow;

use fml_bc_interpreter::*;
use bc::*;
use ast::*;

#[derive(Debug, Clone)]
enum Value {
    Int(i32),
    Bool(bool),
    Function { parameters: Vec<Identifier>, body: Box<AST> },
    Null,
}

impl From<&Value> for Vec<u8> {
    fn from(value: &Value) -> Self {
        let (tag, mut repr) = match value {
            Value::Int(i) => (1, i.to_le_bytes().to_vec()),
            Value::Bool(b) => (2, vec![if *b { 1 } else { 0 }]),
            Value::Function { parameters, body } => {
                let mut bytes = Vec::new();
                bytes.extend_from_slice(&(parameters.len() as u64).to_le_bytes());
                for param in parameters {
                    bytes.extend_from_slice(&param.0.len().to_le_bytes());
                    bytes.extend_from_slice(param.0.as_bytes());
                }
                let serialised = serde_json::to_vec(&*body).unwrap();
                bytes.extend_from_slice((serialised.len() as u64).to_le_bytes().as_slice());
                bytes.extend_from_slice(serialised.as_slice());
                (3, bytes)
            }
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
            3 => {
                let mut repr = bytes;
                let num_params = u64::from_le_bytes(repr[..8].try_into().unwrap());
                repr = &repr[8..];
                let mut parameters = Vec::new();
                for _ in 0..num_params {
                    let len = u64::from_le_bytes(repr[..8].try_into().unwrap());
                    repr = &repr[8..];
                    let param = String::from_utf8(repr[..len as usize].to_vec()).unwrap();
                    parameters.push(Identifier(param));
                    repr = &repr[len as usize..];
                }
                let num_body_bytes = u64::from_le_bytes(repr[..8].try_into().unwrap());
                repr = &repr[8..];
                let mut body_bytes = Vec::new();
                body_bytes.extend_from_slice(&repr[..num_body_bytes as usize]);
                let body = serde_json::from_slice(&body_bytes).unwrap();
                Value::Function { parameters, body }
            }
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
}

impl Display for Value {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Value::Int(i) => write!(fmt, "{}", i),
            Value::Bool(b) => write!(fmt, "{}", b),
            Value::Function { .. } => write!(fmt, "function"),
            Value::Null => write!(fmt, "null"),
        }
    }
}

const STACK_LIMIT: usize = 1024;

type Pc = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
struct StackFrame {
    locals: Vec<usize>,
    return_address: Pc,
}

#[derive(Debug, Default, Clone)]
struct Interpreter {
    constant_pool: Vec<Constant>,
    code: Vec<Instr>,
    env: BTreeMap<Identifier, usize>,
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

        let pc = match &constant_pool[entry_point as usize] {
            Constant::Method { name_idx: _, n_args: _, n_locals: _, start, length: _ } => Ok(*start),
            _ => Err(anyhow!("Invalid entry point")),
        }?;

        Ok(Interpreter {
            env: BTreeMap::new(),
            stack: vec![],
            call_stack: vec![],
            heap: vec![],
            constant_pool,
            code,
            pc,
        })
    }

    fn alloc(&mut self, v: &Value) -> Result<usize> {
        let addr = self.heap.len();
        self.set(addr, v)?;
        Ok(addr)
    }

    fn bind(&mut self, id: &Identifier, addr: usize) -> Result<()> {
        self.env.insert(id.clone(), addr);
        Ok(())
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

    fn frame(&self) -> StackFrame {
        self.call_stack.last().unwrap().clone()
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

    fn execute(&mut self) -> Result<Value> {
        if self.pc >= self.code.len() {
            return Ok(Value::Null)
            // return Err(anyhow!("program counter out of bounds"))
        }

        match self.code[self.pc] {
            Instr::Literal(i) => {
                self.push(match self.constant_pool[i as usize] {
                    Constant::Null => Ok(Value::Null),
                    Constant::Boolean(b) => Ok(Value::Bool(b)),
                    Constant::Integer(n) => Ok(Value::Int(n)),
                    Constant::String(_) => Err(anyhow!("attempt to push a string onto the stack")),
                    Constant::Slot(_) => Err(anyhow!("attempt to push a slot onto the stack")),
                    Constant::Method { .. } => Err(anyhow!("attempt to push a method onto the stack")),
                }?)?;
                Ok(Value::Null) as Result<Value>
            },
            Instr::Drop => {
                self.pop()?;
                Ok(Value::Null)
            },
            Instr::Print(idx, n_args) => {
                let args = (0..n_args).map(|_| self.pop()).collect::<Result<Vec<_>>>()?;
                let format_string = self.constant_pool[idx as usize].as_string()?;
                self.print(&format_string, &args.into_iter().rev().collect_vec())?;
                self.push(Value::Null)?;
                Ok(Value::Null)
            },
            Instr::GetLocal(i) => {
                let &addr = self.frame().locals.get(i as usize).ok_or_else(|| anyhow!("index {} out of range", i))?;
                self.push(self.get(addr)?)?;
                Ok(Value::Null)
            },
            Instr::SetLocal(_) => unimplemented!(),
            Instr::GetGlobal(_) => unimplemented!(),
            Instr::SetGlobal(_) => unimplemented!(),
            Instr::Label(_) => unimplemented!(),
            Instr::Jump(_) => unimplemented!(),
            Instr::Branch(_) => unimplemented!(),
            Instr::CallFunction(_, _) => unimplemented!(),
            Instr::Return => unimplemented!(),
        }?;

        self.pc += 1;
        self.execute()
    }

    fn integer(i: i32)  -> Value { Value::Int(i) }
    fn boolean(b: bool) -> Value { Value::Bool(b) }
    fn null()           -> Value { Value::Null }

    // fn variable(&mut self, name: &Identifier, value: &AST) -> Result<Value> {
    //     let v = self.eval(value)?;
    //     let addr = self.alloc(&v)?;
    //     self.env.insert(name.clone(), addr);
    //     Ok(v)
    // }

    fn array(&mut self, size: &AST, value: &AST) -> Result<Value> {
        todo!()
    }

    fn object(extends: &AST, members: &[AST]) -> Result<Value> {
        todo!()
    }

    fn access_field(&mut self, object: &AST, field: &Identifier) -> Result<Value> {
        todo!()
    }

    fn access_array(array: &AST, index: &AST) -> Result<Value> {
        todo!()
    }

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

    fn assign_field(object: &AST, field: &Identifier, value: &AST) -> Result<Value> {
        todo!()
    }

    fn assign_array(array: &AST, index: &AST, value: &AST) -> Result<Value> {
        todo!()
    }

    fn function(&mut self, name: &Identifier, parameters: &[Identifier], body: &AST) -> Result<()> {
        let f = Value::Function { parameters: parameters.to_vec(), body: Box::new(body.clone()) };
        let addr = self.alloc(&f)?;
        self.bind(name, addr)
    }

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

    fn call_method(object: &AST, name: &Identifier, arguments: &[AST]) -> Result<Value> {
        todo!()
    }

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
    Interpreter::load(&mut std::io::stdin())?.execute()?;
    Ok(())
}