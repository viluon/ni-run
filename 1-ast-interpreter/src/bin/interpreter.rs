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
    fn alloc(&mut self, v: Value) -> Result<usize> {
        let addr = self.heap.len();
        self.heap.insert(addr, v);
        Ok(addr)
    }

    fn lookup(&mut self, id: &Identifier) -> Result<&Value> {
        match self.env.get(id) {
            Some(addr) => match self.heap.get(addr) {
                Some(v) => Ok(v),
                None => Err(anyhow!("interpreter bug: {} points to address {} (outside heap)", id.0, addr)),
            },
            None => Err(anyhow!(
                "it's generally considered smarter to define variables like {} before trying to access them.", id.0
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
        todo!()
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
                I::assign_variable(name, &**value).map(|()| Value::Null),
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
                self.conditional(&**condition, &**consequent, &**alternative).map(|()| Value::Null),
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

    fn assign_variable(name: &Identifier, value: &AST) -> Result<()> {
        todo!()
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
                    "you should learn to count before writing a function like {}. It takes {} arguments, not {}.",
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
            v => Err(anyhow!("{} is not a function (cannot call {})", name.0, v)),
        }
    }

    fn call_method(object: &AST, name: &Identifier, arguments: &[AST]) -> Result<Value> {
        todo!()
    }

    fn loop_de_loop(&mut self, condition: &AST, body: &AST) -> Result<()> {
        while (self.eval(condition)?).as_bool(|v| anyhow!(
            "loop condition must be a boolean. Do you think {} is a boolean?", v
        ))? {
            self.eval(body)?;
        }
        Ok(())
    }

    fn conditional(&mut self, condition: &AST, consequent: &AST, alternative: &AST) -> Result<()> {
        match self.eval(condition)? {
            Value::Bool(true) => self.eval(consequent),
            Value::Bool(false) => self.eval(alternative),
            v => Err(anyhow!("you're trying to branch on a {}. Do you really think that's a good idea?", v)),
        }?;
        Ok(())
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
            Some(err) => Err(anyhow!(err)),
            None if escape => Err(anyhow!("invalid escape sequence: \\")),
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
    Interpreter::new().eval(ast)?;
    Ok(())
}
