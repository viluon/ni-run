use std::{io::Read, collections::BTreeMap, fmt::Display};
use anyhow::Result;

use fml_ast_interpreter::*;
use ast::{*};

// derive display
#[derive(Debug, Clone)]
enum Value {
    Int(i32),
    Bool(bool),
    Function { parameters: Vec<Identifier>, body: Box<AST> },
    Null,
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

#[derive(Clone)]
enum Interpreter<T> {
    Valid {result: T, env: BTreeMap<Identifier, usize>, heap: BTreeMap<usize, Value>},
    Err {msg: String},
}

impl Interpreter<usize> {
    fn alloc(v: Value) -> Interpreter<usize> {
        Interpreter::Valid { result: 0, env: BTreeMap::new(), heap: BTreeMap::new() }.fast_alloc(v)
    }
}

impl Interpreter<()> {
    fn set_env(env: BTreeMap<Identifier, usize>) -> Interpreter<()> {
        Interpreter::Valid { result: (), env, heap: BTreeMap::new() }
    }

    fn set_heap(heap: BTreeMap<usize, Value>) -> Interpreter<()> {
        Interpreter::Valid { result: (), env: BTreeMap::new(), heap }
    }
}

impl<A> Interpreter<A> where A: Clone {
    fn ret(result: A) -> Interpreter<A> {
        Interpreter::Valid {result, env: BTreeMap::new(), heap: BTreeMap::new()}
    }

    fn fast_ret<B>(&self, result: B) -> Interpreter<B> {
        match (*self).clone() {
            Interpreter::Valid {result: _, env, heap} => Interpreter::Valid {result, env, heap},
            Interpreter::Err {msg} => Interpreter::Err {msg},
        }
    }

    fn crash(msg: String) -> Interpreter<A> {
        Interpreter::Err { msg }
    }

    fn lift<B>(self, f: fn(A) -> Result<B>) -> Interpreter<B> {
        match self {
            Interpreter::Valid { result, env, heap } => {
                match f(result) {
                    Ok(b) => Interpreter::Valid { result: b, env, heap },
                    Err(e) => Interpreter::Err { msg: e.to_string() },
                }
            },
            Interpreter::Err { msg } => Interpreter::Err { msg },
        }
    }

    fn map<F, B>(self, f: F) -> Interpreter<B> where F: FnOnce(A) -> B {
        match self {
            Interpreter::Valid { result, env, heap } => Interpreter::Valid { result: f(result), env, heap },
            Interpreter::Err { msg } => Interpreter::Err { msg },
        }
    }

    fn flat_map<F, B>(self, f: F) -> Interpreter<B> where F: FnOnce(A) -> Interpreter<B> {
        match self {
            Interpreter::Valid { result, mut env, mut heap } => {
                match f(result) {
                    Interpreter::Valid {
                        result: b,
                        env: next_env,
                        heap: next_heap
                    } => {
                        let offset = heap.len();
                        env.extend(next_env.into_iter().map(|(id, addr)| (id, offset + addr)));
                        heap.extend(next_heap.into_iter().map(|(addr, v)| (addr + offset, v)));
                        Interpreter::Valid { result: b, env, heap }
                    },
                    Interpreter::Err { msg } => Interpreter::Err { msg },
                }
            },
            Interpreter::Err { msg } => Interpreter::Err { msg },
        }
    }

    fn fast_alloc(&self, v: Value) -> Interpreter<usize> {
        match (*self).clone() {
            Interpreter::Valid { result, env, mut heap } => {
                let addr = heap.len();
                heap.insert(addr, v);
                Interpreter::Valid { result: addr, env, heap }
            },
            Interpreter::Err { msg } => Interpreter::Err { msg },
        }
    }

    fn env(&self) -> Interpreter<BTreeMap<Identifier, usize>> {
        match (*self).clone() {
            Interpreter::Valid { result, env, heap } => Interpreter::Valid { result: env.clone(), env, heap },
            Interpreter::Err { msg } => Interpreter::Err { msg },
        }
    }

    fn heap(&self) -> Interpreter<BTreeMap<usize, Value>> {
        match (*self).clone() {
            Interpreter::Valid { result, env, heap } => Interpreter::Valid { result: heap.clone(), env, heap },
            Interpreter::Err { msg } => Interpreter::Err { msg },
        }
    }

    fn lookup(&self, id: &Identifier) -> Interpreter<usize> {
        match (*self).clone() {
            Interpreter::Valid { result: addr, env, heap } => {
                match env.get(id) {
                    Some(&addr) => Interpreter::Valid { result: addr, env, heap },
                    None => I::crash(format!("undefined identifier {}", id.0)),
                }
            },
            Interpreter::Err { msg } => Interpreter::Err { msg },
        }
    }
}

impl Default for Interpreter<()> {
    fn default() -> Self {
        Interpreter::Valid {
            result: (),
            env: BTreeMap::new(),
            heap: BTreeMap::new(),
        }
    }
}

use Interpreter as I;

impl<A> Interpreter<A> where A: Clone {
    fn integer(
        &self, i: i32
    ) -> Interpreter<Value> {
        self.fast_ret(Value::Int(i))
    }

    fn boolean(
        &self, b: bool
    ) -> Interpreter<Value> {
        self.fast_ret(Value::Bool(b))
    }

    fn null(
        &self
    ) -> Interpreter<Value> {
        self.fast_ret(Value::Null)
    }

    fn variable(
        &self, name: &Identifier, value: &AST
    ) -> Interpreter<()> {
        self.eval(value).flat_map(|v| {
            I::alloc(v).flat_map(|addr| {
                self.env().flat_map(|mut env| {
                    env.insert(name.clone(), addr);
                    I::set_env(env)
                })
            })
        })
    }

    fn array(
        &self, size: &AST, value: &AST
    ) -> Interpreter<Value> {
        todo!()
    }

    fn object(
        &self, extends: &AST, members: &[AST]
    ) -> Interpreter<Value> {
        todo!()
    }

    fn access_variable(
        &self, name: &Identifier
    ) -> Interpreter<Value> {
        let int = self.env().flat_map(|env| {
            match env.get(name) {
                Some(&addr) => I::ret(addr),
                None => I::crash(format!("variable {} not found", name.0)),
            }
        });
        // FIXME: eugh
        int.clone().flat_map(|addr| {
            int.heap().map(|heap| {
                heap[&addr].clone()
            })
        })
    }

    fn access_field(
        &self, object: &AST, field: &Identifier
    ) -> Interpreter<Value> {
        todo!()
    }

    fn access_array(
        &self, array: &AST, index: &AST
    ) -> Interpreter<Value> {
        todo!()
    }

    fn assign_variable(
        &self, name: &Identifier, value: &AST
    ) -> Interpreter<()> {
        let int = self.eval(&value).flat_map(|v| {
            self.lookup(name).map(|addr| (addr, v))
        });
        int.clone().flat_map(|(addr, v)| {
            int.heap().flat_map(|mut heap| {
                heap.insert(addr, v);
                I::set_heap(heap)
            })
        })
    }

    fn assign_field(
        &self, object: &AST, field: &Identifier, value: &AST
    ) -> Interpreter<Value> {
        todo!()
    }

    fn assign_array(
        &self, array: &AST, index: &AST, value: &AST
    ) -> Interpreter<Value> {
        todo!()
    }

    fn function(
        &self, name: &Identifier, parameters: &[Identifier], body: &AST
    ) -> Interpreter<()> {
        self.fast_alloc(Value::Function { parameters: parameters.to_vec(), body: Box::new(body.clone()) })
            .flat_map(|addr| {
                self.env().flat_map(|mut env| {
                    env.insert(name.clone(), addr);
                    I::set_env(env)
                })
            })
    }

    fn call_function(
        &self, name: &Identifier, arguments: &[AST]
    ) -> Interpreter<Value> {
        self.lookup(name).flat_map(move |addr| {
            self.heap().flat_map(|heap| {
                match heap.get(&addr) {
                    None => I::crash(format!("function {} not found", name.0)),
                    Some(Value::Function { parameters, body }) => {
                        let mut interpreter = self.fast_ret(());
                        for (parameter, argument) in parameters.into_iter().zip(arguments.into_iter()) {
                            interpreter = interpreter.eval(&argument).flat_map(|v| {
                                interpreter.fast_alloc(v).flat_map(|addr| {
                                    interpreter.env().flat_map(|mut env| {
                                        env.insert(parameter.clone(), addr);
                                        I::set_env(env)
                                    })
                                })
                            });
                        }

                        interpreter.clone().flat_map(|()| {
                            interpreter.eval(&**body)
                        })
                    },
                    Some(v) => I::crash(format!("cannot call {} = {}", name.0, v)),
                }
            })
        })
    }

    fn call_method(
        &self, object: &AST, name: &Identifier, arguments: &[AST]
    ) -> Interpreter<Value> {
        todo!()
    }

    fn top(
        &self, statements: &[AST]
    ) -> Interpreter<Value> {
        let mut interpreter = self.fast_ret(Value::Null);
        for statement in statements {
            interpreter = interpreter.eval(statement);
        }
        interpreter
    }

    fn block(
        &self, statements: &[AST]
    ) -> Interpreter<Value> {
        self.top(statements)
    }

    fn loop_de_loop(
        &self, condition: &AST, body: &AST
    ) -> Interpreter<Value> {
        self.eval(condition).flat_map(|v| {
            match v {
                Value::Bool(true) => self.eval(body).flat_map(|v| {
                    self.loop_de_loop(condition, body)
                }),
                Value::Bool(false) => self.fast_ret(Value::Null),
                _ => I::crash(format!("condition must be a boolean: {}", v)),
            }
        })
    }

    fn conditional(
        &self, condition: &AST, consequent: &AST, alternative: &AST
    ) -> Interpreter<Value> {
        self.eval(condition).flat_map(|v| {
            match v {
                Value::Bool(true) => self.eval(consequent),
                Value::Bool(false) => self.eval(alternative),
                _ => I::crash(format!("condition must be a boolean: {}", v)),
            }
        })
    }

    fn print(
        &self, format: &str, arguments: &[AST]
    ) -> Interpreter<Value> {
        let mut interpreter = self.fast_ret(vec![]);
        for argument in arguments {
            interpreter = interpreter.clone().flat_map(|mut args|
                interpreter.eval(argument).map(|v| {
                    args.push(v);
                    args
                })
            );
        }
        interpreter.flat_map(|args| {
            let mut escape = false;
            let mut str = String::new();
            let mut err = None;
            let mut arg_index = 0;

            for ch in format.chars() {
                match (escape, ch) {
                    (false, '~') => match args.get(arg_index) {
                        Some(arg) => {
                            str.push_str(&arg.to_string());
                            arg_index += 1
                        },
                        None => {
                            err = Some("too few arguments for format string".to_string());
                            break
                        },
                    },
                    (false, '\\') => escape = true,
                    (false, ch) => str.push(ch),
                    (true, '~' | 'n' | '"' | 'r' | 't' | '\\') => {
                        str.push(ch);
                        escape = false
                    },
                    (true, _) => {
                        err = Some(format!("invalid escape sequence: \\{}", ch));
                        break
                    },
                }
            }

            match err {
                Some(err) => I::crash(err),
                None if escape => I::crash("invalid escape sequence: \\".to_string()),
                None => {
                    println!("{}", str);
                    self.fast_ret(Value::Null)
                },
            }
        })
    }

    fn eval(
        &self, ast: &AST
    ) -> Interpreter<Value> {
        match ast {
            AST::Null =>
                self.null(),
            AST::Integer(n) =>
                self.integer(*n),
            AST::Boolean(b) =>
                self.boolean(*b),
            AST::Variable { name, value } =>
                self.variable(name, &**value).map(|()| Value::Null),
            AST::Array { size, value } =>
                self.array(&**size, &**value),
            AST::Object { extends, members } =>
                self.object(&**extends, members),
            AST::AccessVariable { name } =>
                self.access_variable(name),
            AST::AccessField { object, field } =>
                self.access_field(&**object, field),
            AST::AccessArray { array, index } =>
                self.access_array(&**array, &**index),
            AST::AssignVariable { name, value } =>
                self.assign_variable(name, &**value).map(|()| Value::Null),
            AST::AssignField { object, field, value } =>
                self.assign_field(&**object, field, &**value),
            AST::AssignArray { array, index, value } =>
                self.assign_array(&**array, &**index, &**value),
            AST::Function { name, parameters, body } =>
                self.function(name, parameters, &**body).map(|()| Value::Null),
            AST::CallFunction { name, arguments } =>
                self.call_function(name, arguments),
            AST::CallMethod { object, name, arguments } =>
                self.call_method(&**object, name, arguments),
            AST::Top(stmts) =>
                self.top(stmts),
            AST::Block(stmts) =>
                self.block(stmts),
            AST::Loop { condition, body } =>
                self.loop_de_loop(&**condition, &**body),
            AST::Conditional { condition, consequent, alternative } =>
                self.conditional(&**condition, &**consequent, &**alternative),
            AST::Print { format, arguments } =>
                self.print(format, arguments),
        }
    }
}

fn main() -> Result<()> {
    // load json from stdin
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input)?;
    let ast: AST = serde_json::from_str(&input)?;
    let interpreter: Interpreter<()> = Default::default();
    interpreter.eval(&ast);
    Ok(())
}
