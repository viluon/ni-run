#![feature(fn_traits, associated_type_defaults)]

use std::{io::Read, collections::BTreeMap, fmt::Display, rc::Rc};
use anyhow::Result;

use fml_ast_interpreter::*;
use state::*;
use ast::*;

// the state monad adapted for errors with short-circuiting
pub enum Either<A, B> { Left(A), Right(B) }
pub struct ErrorState<'a, E, S, A> {
    pub run_error_state: Box<dyn FnOnce(S) -> Either<E, (A, S)> + 'a>
}

impl<'a, E, S, A> ErrorState<'a, E, S, A> where E: 'a, S: 'a, A: 'a {
    pub fn flat_map<B, F>(self, f: F) -> ErrorState<'a, E, S, B>
    where F: Fn(A) -> ErrorState<'a, E, S, B> + 'a {
        ErrorState {
            run_error_state: Box::new(move |first_state| {
                match self.run_error_state.call_once((first_state,)) {
                    Either::Left(err) => Either::Left(err),
                    Either::Right((result, next_state)) => f(result).run_error_state.call_once((next_state,))
                }
            })
        }
    }

    pub fn map<B, F>(self, f: F) -> ErrorState<'a, E, S, B>
    where F: Fn(A) -> B + 'a, B: 'a {
        self.flat_map(move |a| ErrorState::pure(f(a)))
    }

    pub fn pure(a: A) -> ErrorState<'a, E, S, A> {
        ErrorState {
            run_error_state: Box::new(move |s| {
                Either::Right((a, s))
            })
        }
    }

    pub fn error(e: E) -> ErrorState<'a, E, S, A> {
        ErrorState {
            run_error_state: Box::new(move |_| {
                Either::Left(e)
            })
        }
    }
}

impl<'a, E, S> ErrorState<'a, E, S, S> where S: Clone {
    pub fn get() -> ErrorState<'a, E, S, S> {
        ErrorState {
            run_error_state: Box::new(move |s| {
                Either::Right((s.clone(), s))
            })
        }
    }
}

impl<'a, E, S> ErrorState<'a, E, S, ()> where S: 'a {
    pub fn put(s: S) -> ErrorState<'a, E, S, ()> {
        ErrorState {
            run_error_state: Box::new(move |_| {
                Either::Right(((), s))
            })
        }
    }
}


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

#[derive(Debug, Default, Clone)]
struct InterpreterState {
    env: BTreeMap<Identifier, usize>,
    heap: BTreeMap<usize, Value>,
}

type Interpreter<'a, A> = ErrorState<'a, String, InterpreterState, A>;
use Interpreter as I;

impl<'a> Interpreter<'a, usize> {
    fn alloc(v: Value) -> Interpreter<'a, usize> {
        // Interpreter::Valid { result: 0, env: BTreeMap::new(), heap: BTreeMap::new() }.fast_alloc(v)
        todo!()
    }

    fn lookup(id: &'a Identifier) -> Interpreter<'a, usize> {
        // match (*self).clone() {
        //     Interpreter::Valid { result: addr, env, heap } => {
        //         match env.get(id) {
        //             Some(&addr) => Interpreter::Valid { result: addr, env, heap },
        //             None => I::crash(format!("undefined identifier {}", id.0)),
        //         }
        //     },
        //     Interpreter::Err { msg } => Interpreter::Err { msg },
        // }
        todo!()
    }
}

impl<'a> Interpreter<'a, ()> {
    fn set_env(env: BTreeMap<Identifier, usize>) -> Interpreter<'a, ()> {
        // Interpreter::Valid { result: (), env, heap: BTreeMap::new() }
        todo!()
    }

    fn set_heap(heap: BTreeMap<usize, Value>) -> Interpreter<'a, ()> {
        // Interpreter::Valid { result: (), env: BTreeMap::new(), heap }
        todo!()
    }
}

impl<'a> Interpreter<'a, BTreeMap<Identifier, usize>> {
    fn env() -> Interpreter<'a, BTreeMap<Identifier, usize>> {
        I::get().map(|s| s.env)
    }
}

impl<'a> Interpreter<'a, BTreeMap<usize, Value>> {
    fn heap() -> Interpreter<'a, BTreeMap<usize, Value>> {
        I::get().map(|s| s.heap)
    }
}

impl<'a, A> Interpreter<'a, A> where A: Clone {
    fn ret(result: A) -> Interpreter<'a, A> {
        // Interpreter::Valid {result, env: BTreeMap::new(), heap: BTreeMap::new()}
        todo!()
    }

    fn fast_ret<B>(&self, result: B) -> Interpreter<'a, B> {
        // match (*self).clone() {
        //     Interpreter::Valid {result: _, env, heap} => Interpreter::Valid {result, env, heap},
        //     Interpreter::Err {msg} => Interpreter::Err {msg},
        // }
        todo!()
    }

    fn crash(msg: String) -> Interpreter<'a, A> {
        // Interpreter::Err { msg }
        todo!()
    }

    fn fast_alloc(&self, v: Value) -> Interpreter<'a, usize> {
        // match (*self).clone() {
        //     Interpreter::Valid { result, env, mut heap } => {
        //         let addr = heap.len();
        //         heap.insert(addr, v);
        //         Interpreter::Valid { result: addr, env, heap }
        //     },
        //     Interpreter::Err { msg } => Interpreter::Err { msg },
        // }
        todo!()
    }
}

impl<'a> Default for Interpreter<'a, ()> {
    fn default() -> Self { I::pure(()) }
}

impl<'a> Interpreter<'a, Value> {
    fn top(
        statements: &'a [AST]
    ) -> Interpreter<'a, Value> {
        statements.iter().fold(I::pure(Value::Null), move |interpreter, statement| {
            interpreter.flat_map(move |_| I::eval(statement))
        })
    }

    fn block(
        statements: &'a [AST]
    ) -> Interpreter<'a, Value> {
        I::top(statements)
    }

    fn eval(
        ast: &'a AST
    ) -> Interpreter<'a, Value> {
        match ast {
            AST::Null =>
                I::null(),
            AST::Integer(n) =>
                I::integer(*n),
            AST::Boolean(b) =>
                I::boolean(*b),
            AST::Variable { name, value } =>
                I::variable(name, &**value).map(|()| Value::Null),
            AST::Array { size, value } =>
                I::array(&**size, &**value),
            AST::Object { extends, members } =>
                I::object(&**extends, members),
            AST::AccessVariable { name } =>
                I::access_variable(name),
            AST::AccessField { object, field } =>
                I::access_field(&**object, field),
            AST::AccessArray { array, index } =>
                I::access_array(&**array, &**index),
            AST::AssignVariable { name, value } =>
                I::assign_variable(name, &**value).map(|()| Value::Null),
            AST::AssignField { object, field, value } =>
                I::assign_field(&**object, field, &**value),
            AST::AssignArray { array, index, value } =>
                I::assign_array(&**array, &**index, &**value),
            AST::Function { name, parameters, body } =>
                I::function(name, parameters, &**body).map(|()| Value::Null),
            AST::CallFunction { name, arguments } =>
                I::call_function(name, arguments),
            AST::CallMethod { object, name, arguments } =>
                I::call_method(&**object, name, arguments),
            AST::Top(stmts) =>
                I::top(stmts),
            AST::Block(stmts) =>
                I::block(stmts),
            AST::Loop { condition, body } =>
                I::loop_de_loop(&**condition, &**body),
            AST::Conditional { condition, consequent, alternative } =>
                I::conditional(&**condition, &**consequent, &**alternative),
            AST::Print { format, arguments } =>
                I::print(format, arguments),
        }
    }

    fn integer(
        i: i32
    ) -> Interpreter<'a, Value> {
        I::pure(Value::Int(i))
    }

    fn boolean(
        b: bool
    ) -> Interpreter<'a, Value> {
        I::pure(Value::Bool(b))
    }

    fn null(
    ) -> Interpreter<'a, Value> {
        I::pure(Value::Null)
    }

    fn variable(
        name: &'a Identifier, value: &'a AST
    ) -> Interpreter<'a, ()> {
        I::eval(value).flat_map(move |v| {
            I::alloc(v).flat_map(move |addr| {
                I::env().flat_map(move |mut env| {
                    env.insert(name.clone(), addr);
                    I::set_env(env)
                })
            })
        })
    }

    fn array(
        size: &'a AST, value: &'a AST
    ) -> Interpreter<'a, Value> {
        todo!()
    }

    fn object(
        extends: &'a AST, members: &[AST]
    ) -> Interpreter<'a, Value> {
        todo!()
    }

    fn access_variable(
        name: &Identifier
    ) -> Interpreter<'a, Value> {
        let int = I::env().flat_map(|env| {
            match env.get(name) {
                Some(&addr) => I::ret(addr),
                None => I::crash(format!("variable {} not found", name.0)),
            }
        });
        // FIXME: eugh
        // int.clone().flat_map(|addr| {
        //     int.heap().map(|heap| {
        //         heap[&addr].clone()
        //     })
        // })
        todo!()
    }

    fn access_field(
        object: &'a AST, field: &Identifier
    ) -> Interpreter<'a, Value> {
        todo!()
    }

    fn access_array(
        array: &'a AST, index: &'a AST
    ) -> Interpreter<'a, Value> {
        todo!()
    }

    fn assign_variable(
        name: &'a Identifier, value: &'a AST
    ) -> Interpreter<'a, ()> {
        I::eval(&value).flat_map(move |v| {
            I::lookup(name).flat_map(move |addr| {
                // FIXME weird borrowck (likely lifetime inference) issues,
                // claiming that v is stuck captured in a Fn closure (despite
                // all the moves)
                let v = v.clone();
                I::heap().flat_map(move |mut heap| {
                    heap.insert(addr, v.clone());
                    I::set_heap(heap)
                })
            })
        })
    }

    fn assign_field(
        object: &'a AST, field: &Identifier, value: &'a AST
    ) -> Interpreter<'a, Value> {
        todo!()
    }

    fn assign_array(
        array: &'a AST, index: &'a AST, value: &'a AST
    ) -> Interpreter<'a, Value> {
        todo!()
    }

    fn function(
        name: &'a Identifier, parameters: &'a [Identifier], body: &'a AST
    ) -> Interpreter<'a, ()> {
        // Value::Function { parameters: parameters.to_vec(), body: Box::new(body.clone()) }
        // self.fast_alloc(todo!())
        //     .flat_map(|addr| {
        //         I::env().flat_map(move |mut env| {
        //             env.insert(name.clone(), addr);
        //             I::set_env(env)
        //         })
        //     })
        todo!()
    }

    fn call_function(
        name: &'a Identifier, arguments: &'a [AST]
    ) -> Interpreter<'a, Value> {
        I::lookup(name).flat_map(move |addr| {
            I::heap().flat_map(move |heap| {
                match heap.get(&addr) {
                    None => I::crash(format!("function {} not found", name.0)),
                    Some(Value::Function { parameters, body }) => {
                        let mut interpreter = I::pure(());
                        for (parameter, argument) in parameters.into_iter().zip(arguments.into_iter()) {
                            // interpreter = interpreter.eval(&argument).flat_map(move |v| {
                            //     let p = parameter.clone();
                            //     interpreter.fast_alloc(v).flat_map(move |addr| {
                            //         I::env().flat_map(move |mut env| {
                            //             env.insert(p, addr);
                            //             I::set_env(env)
                            //         })
                            //     })
                            // });
                            todo!()
                        }

                        // interpreter.clone().flat_map(|()| {
                        //     interpreter.eval(&**body)
                        // })
                        todo!()
                    },
                    Some(v) => I::crash(format!("cannot call {} = {}", name.0, v)),
                }
            })
        })
    }

    fn call_method(
        object: &'a AST, name: &Identifier, arguments: &[AST]
    ) -> Interpreter<'a, Value> {
        todo!()
    }

    fn loop_de_loop(
        condition: &'a AST, body: &'a AST
    ) -> Interpreter<'a, Value> {
        I::eval(condition).flat_map(|v| {
            match v {
                Value::Bool(true) => I::eval(body).flat_map(|v| {
                    I::loop_de_loop(condition, body)
                }),
                Value::Bool(false) => I::pure(Value::Null),
                _ => I::crash(format!("condition must be a boolean: {}", v)),
            }
        })
    }

    fn conditional(
        condition: &'a AST, consequent: &'a AST, alternative: &'a AST
    ) -> Interpreter<'a, Value> {
        I::eval(condition).flat_map(|v| {
            match v {
                Value::Bool(true) => I::eval(consequent),
                Value::Bool(false) => I::eval(alternative),
                _ => I::crash(format!("condition must be a boolean: {}", v)),
            }
        })
    }

    fn print(
        format: &'a str, arguments: &'a [AST]
    ) -> Interpreter<'a, Value> {
        let interpreter =
            arguments.iter().fold(I::pure(vec![]), |interpreter, arg| {
                interpreter.flat_map(move |args|
                    I::eval(arg).map(move |v| {
                        let mut args = args.clone();
                        args.push(v);
                        args
                    })
                )
            });

        interpreter.flat_map(move |args| {
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
                    I::pure(Value::Null)
                },
            }
        })
    }
}

fn main() -> Result<()> {
    // load json from stdin
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input)?;
    let ast: AST = serde_json::from_str(&input)?;
    (I::eval(&ast).run_error_state)(InterpreterState::default());
    Ok(())
}
