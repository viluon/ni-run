#![feature(fn_traits, associated_type_defaults)]

use std::{io::Read, collections::BTreeMap, fmt::Display, rc::Rc};
use anyhow::Result;
use anyhow::anyhow;

use fml_ast_interpreter::*;
use state::*;
use ast::*;

// the state monad adapted for errors with short-circuiting
pub enum Either<A, B> { Left(A), Right(B) }
pub struct ErrorState<'a, E, S, A> {
    pub run_error_state: Box<dyn FnOnce(S) -> Either<E, (A, S)> + 'a>
}

impl<'a, E, S, A> ErrorState<'a, E, S, A> where E: 'a, S: 'a, A: 'a {
    pub fn flat_map<'b, B, F>(&'b self, f: F) -> ErrorState<'b, E, S, B>
    where F: FnOnce(A) -> ErrorState<'b, E, S, B> + 'b {
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
    where F: FnOnce(A) -> B + 'a, B: 'a {
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
        I::heap().flat_map(move |mut heap| {
            let id = heap.len();
            heap.insert(id, v.clone());
            I::set_heap(heap).map(move |_| id)
        })
    }

    fn lookup(id: &'a Identifier) -> Interpreter<'a, usize> {
        I::env().flat_map(move |env| {
            match env.get(id) {
                Some(&addr) => I::pure(addr),
                None => I::error(format!("undefined variable: {}", id.0))
            }
        })
    }
}

impl<'a> Interpreter<'a, ()> {
    // FIXME unnecessary cloning because borrowck is dumb
    fn set_env(env: BTreeMap<Identifier, usize>) -> Interpreter<'a, ()> {
        I::get().flat_map(move |s| I::put(InterpreterState { env: env.clone(), ..s }))
    }

    fn set_heap(heap: BTreeMap<usize, Value>) -> Interpreter<'a, ()> {
        I::get().flat_map(move |s| I::put(InterpreterState { heap: heap.clone(), ..s }))
    }

    fn bind(id: Identifier, addr: usize) -> Interpreter<'a, ()> {
        I::env().flat_map(move |mut env| {
            env.insert(id.clone(), addr);
            I::set_env(env)
        })
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
                Some(&addr) => I::pure(addr),
                None => I::error(format!("variable {} not found", name.0)),
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
        let f = Value::Function { parameters: parameters.to_vec(), body: Box::new(body.clone()) };
        I::alloc(f).flat_map(move |addr| {
            I::env().flat_map(move |mut env| {
                env.insert(name.clone(), addr);
                I::set_env(env)
            })
        })
    }

    fn call_function<'b>(
        name: &'a Identifier, arguments: &'a [AST]
    ) -> Interpreter<'b, Value> {
        let perform_call = move |parameters: &'a Vec<Identifier>, body: &Box<AST>| -> Interpreter<'b, Value> {
            parameters.iter().zip(arguments.iter().map(Box::new))
                .fold(I::pure(body.clone()), move |interpreter, (param, arg)| {
                    let param = param.clone();
                    let arg = arg.clone();
                    interpreter.flat_map(move |body| {
                        I::eval(&*arg).flat_map(move |v| {
                            I::alloc(v).flat_map(move |addr| {
                                I::bind(param.clone(), addr).map(move |()| body)
                            })
                        })
                    })
                }).flat_map(move |body| {
                    I::eval(&*body)
                })
        };

        I::lookup(name).flat_map(move |addr| {
            I::heap().flat_map(move |heap| {
                match heap.get(&addr) {
                    None => I::error(format!("function {} not found", name.0)),
                    Some(Value::Function { parameters, body }) => {
                        perform_call(parameters, body)
                    },
                    Some(v) => I::error(format!("cannot call {} (bound to {}), it is not a function", v, name.0)),
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
                _ => I::error(format!("condition must be a boolean: {}", v)),
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
                _ => I::error(format!("condition must be a boolean: {}", v)),
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
                Some(err) => I::error(err),
                None if escape => I::error("invalid escape sequence: \\".to_string()),
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
    let ast = serde_json::from_str(&input)?;
    let result = (I::eval(&ast).run_error_state)(InterpreterState::default());
    match result {
        Either::Left(msg) => Err(anyhow!(msg)),
        Either::Right((v, _s)) => Ok(println!("finished with: {}", v)),
    }
}
