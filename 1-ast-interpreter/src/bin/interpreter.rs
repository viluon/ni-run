#![feature(fn_traits, associated_type_defaults)]

use std::{io::Read, collections::BTreeMap, fmt::Display, rc::Rc};
use anyhow::Result;
use anyhow::anyhow;

use fml_ast_interpreter::*;
use state::*;
use ast::*;

#[derive(Debug, Clone)]
enum Value {
    Int(i32),
    Bool(bool),
    Function { parameters: Vec<Identifier>, body: Box<AST> },
    Null,
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

#[derive(Debug, Default, Clone)]
struct Interpreter {
    env: BTreeMap<Identifier, usize>,
    heap: BTreeMap<usize, Value>,
}

use Interpreter as I;

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            env: BTreeMap::new(),
            heap: BTreeMap::new(),
        }
    }

    fn alloc(&mut self, v: Value) -> Result<usize> {
        let addr = self.heap.len();
        self.heap.insert(addr, v);
        Ok(addr)
    }

    fn lookup(&mut self, id: &Identifier) -> Result<&Value> {
        match self.env.get(id) {
            Some(addr) => match self.heap.get(addr) {
                Some(v) => Ok(v),
                None => Err(anyhow!(
                    "interpreter bug: {} points to address {} (outside the heap). \
                    Whoever wrote this thing screwed up, big time.", id.0, addr
                )),
            },
            None => Err(anyhow!(
                "it's generally considered smarter to define variables \
                (such as {}) before trying to access them.", id.0
            ))
        }
    }

    fn bind(&mut self, id: &Identifier, addr: usize) -> Result<()> {
        self.env.insert(id.clone(), addr);
        Ok(())
    }
}

impl Interpreter {
    fn top(&mut self, statements: &[AST]) -> Result<Value> {
        statements.iter().fold(Ok(Value::Null), |acc, s| acc.and(self.eval(s)))
    }

    fn block(&mut self, statements: &[AST]) -> Result<Value> {
        self.top(statements)
    }

    fn eval(&mut self, ast: &AST) -> Result<Value> {
        match ast {
            AST::Null =>
                Ok(I::null()),
            AST::Integer(n) =>
                Ok(I::integer(*n)),
            AST::Boolean(b) =>
                Ok(I::boolean(*b)),
            AST::Variable { name, value } =>
                self.variable(name, &**value).map(|()| Value::Null),
            AST::Array { size, value } =>
                self.array(&**size, &**value),
            AST::Object { extends, members } =>
                I::object(&**extends, members),
            AST::AccessVariable { name } =>
                self.access_variable(name),
            AST::AccessField { object, field } =>
                self.access_field(&**object, field),
            AST::AccessArray { array, index } =>
                I::access_array(&**array, &**index),
            AST::AssignVariable { name, value } =>
                self.assign_variable(name, &**value).map(|()| Value::Null),
            AST::AssignField { object, field, value } =>
                I::assign_field(&**object, field, &**value),
            AST::AssignArray { array, index, value } =>
                I::assign_array(&**array, &**index, &**value),
            AST::Function { name, parameters, body } =>
                self.function(name, parameters, &**body).map(|()| Value::Null),
            AST::CallFunction { name, arguments } =>
                self.call_function(name, arguments),
            AST::CallMethod { object, name, arguments } =>
                I::call_method(&**object, name, arguments),
            AST::Top(stmts) =>
                self.top(stmts),
            AST::Block(stmts) =>
                self.block(stmts),
            AST::Loop { condition, body } =>
                self.loop_de_loop(&**condition, &**body).map(|()| Value::Null),
            AST::Conditional { condition, consequent, alternative } =>
                self.conditional(&**condition, &**consequent, &**alternative),
            AST::Print { format, arguments } =>
                self.print(format, arguments),
        }
    }

    fn integer(i: i32)  -> Value { Value::Int(i) }
    fn boolean(b: bool) -> Value { Value::Bool(b) }
    fn null()           -> Value { Value::Null }

    fn variable(&mut self, name: &Identifier, value: &AST) -> Result<()> {
        let v = self.eval(value)?;
        let addr = self.alloc(v)?;
        self.env.insert(name.clone(), addr);
        Ok(())
    }

    fn array(&mut self, size: &AST, value: &AST) -> Result<Value> {
        todo!()
    }

    fn object(extends: &AST, members: &[AST]) -> Result<Value> {
        todo!()
    }

    fn access_variable(&mut self, name: &Identifier) -> Result<Value> {
        self.lookup(name).cloned()
    }

    fn access_field(&mut self, object: &AST, field: &Identifier) -> Result<Value> {
        todo!()
    }

    fn access_array(array: &AST, index: &AST) -> Result<Value> {
        todo!()
    }

    fn assign_variable(&mut self, name: &Identifier, value: &AST) -> Result<()> {
        let v = self.eval(value)?;
        match self.env.get(name) {
            Some(addr) => {
                self.heap.insert(*addr, v);
                Ok(())
            },
            None => Err(anyhow!(
                "you don't seem to understand how variable assignments work. {} is not defined.", name.0
            )),
        }
    }

    fn assign_field(object: &AST, field: &Identifier, value: &AST) -> Result<Value> {
        todo!()
    }

    fn assign_array(array: &AST, index: &AST, value: &AST) -> Result<Value> {
        todo!()
    }

    fn function(&mut self, name: &Identifier, parameters: &[Identifier], body: &AST) -> Result<()> {
        let f = Value::Function { parameters: parameters.to_vec(), body: Box::new(body.clone()) };
        let addr = self.alloc(f)?;
        self.bind(name, addr)
    }

    fn call_function(&mut self, name: &Identifier, arguments: &[AST]) -> Result<Value> {
        match self.lookup(name)?.clone() {
            Value::Function { parameters, .. } if parameters.len() != arguments.len() =>
                Err(anyhow!(
                    "it helps to learn how to count before writing a function like {}. It takes {} arguments, not {}.",
                    name.0,
                    parameters.len(),
                    arguments.len()
                )),
            Value::Function { parameters, body } => {
                let env = self.env.clone();
                parameters
                    .iter()
                    .zip(arguments.iter())
                    .map(|(n, expr)| self.eval(expr).map(|v| (n, v)))
                    .collect::<Result<Vec<_>>>()?
                    .into_iter()
                    .try_for_each::<_, Result<()>>(|(name, v)| {
                        let addr = self.alloc(v)?;
                        self.bind(name, addr)?;
                        Ok(())
                    })?;

                let result = self.eval(&*body)?;
                self.env = env;
                Ok(result)
            },
            v => Err(anyhow!(
                "you tried to call {}, but it didn't work. Maybe you didn't try hard enough, or \
                maybe the stars aren't aligned right today, or maybe it's the fact that {} holds \
                a value of {}, not a function. Who knows?", name.0, name.0, v
            )),
        }
    }

    fn call_method(object: &AST, name: &Identifier, arguments: &[AST]) -> Result<Value> {
        todo!()
    }

    fn loop_de_loop(&mut self, condition: &AST, body: &AST) -> Result<()> {
        while (self.eval(condition)?).as_bool(|v| anyhow!(
            "the difference between your program and most others \
            is that other people tend to put booleans as their loop conditions. \
            Using a value of {} instead may just become the next big thing. Keep at it.", v
        ))? {
            self.eval(body)?;
        }
        Ok(())
    }

    fn conditional(&mut self, condition: &AST, consequent: &AST, alternative: &AST) -> Result<Value> {
        match self.eval(condition)? {
            Value::Bool(true) => self.eval(consequent),
            Value::Bool(false) => self.eval(alternative),
            v => Err(anyhow!("you're trying to branch on a {}. Do you really think that's a good idea?", v)),
        }
    }

    fn print(&mut self, format: &str, arguments: &[AST]) -> Result<Value> {
        let mut args = vec![];
        for arg in arguments {
                let v = self.eval(arg)?;
                args.push(v)
        }

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
                println!("{}", str);
                Ok(Value::Null)
            },
        }
    }
}

fn main() -> Result<()> {
    // load json from stdin
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input)?;
    let ast = serde_json::from_str(&input)?;
    Interpreter::new().eval(&ast)?;
    Ok(())
}
