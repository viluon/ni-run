use std::{io::Read, collections::BTreeMap};
use anyhow::Result;

use fml_ast_interpreter::*;
use ast::{*, fold::*};

#[derive(Debug, Clone)]
enum Value {
    Int(i32),
    Bool(bool),
    Null,
}

struct Interpreter {
    result: Value,
    env: BTreeMap<Identifier, usize>,
    heap: Vec<Value>,
    msg: Option<String>,
}

impl Interpreter {
    // helper for updating the result of interpretation while respecting short-circuits
    fn insert(&mut self, result: Value) -> AST {
        if self.msg.is_none() {
            self.result = result;
        }
        AST::Null
    }

    fn crash(&mut self, msg: String) -> AST {
        self.msg = Some(msg);
        AST::Null
    }

    fn alloc(&mut self, v: Value) -> usize {
        let idx = self.heap.len();
        self.heap.push(v);
        idx
    }

    fn new() -> Self { Default::default() }
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter {
            result: Value::Null,
            env: BTreeMap::new(),
            heap: vec![],
            msg: None,
        }
    }
}

impl AstFold for Interpreter {
    fn integer(&mut self, n: i32) -> AST {
        self.insert(Value::Int(n))
    }

    fn boolean(&mut self, b: bool) -> AST {
        self.insert(Value::Bool(b))
    }

    fn null(&mut self) -> AST {
        self.insert(Value::Null)
    }

    fn variable(&mut self, name: Identifier, value: AST) -> AST {
        self.fold(value);
        let addr = self.alloc(self.result.clone());
        self.env.insert(name, addr);
        AST::Null
    }

    fn access_variable(&mut self, name: Identifier) -> AST {
        match self.env.get(&name) {
            Some(&v) => self.insert(self.heap[v].clone()),
            None => self.crash(format!("undefined variable {}", name.0)),
        }
    }

    fn assign_variable(&mut self, name: Identifier, value: AST) -> AST {
        let addr = self.env[&name];
        self.fold(value);
        self.heap[addr] = self.result.clone();
        AST::Null
    }

    fn function(&mut self, name: Identifier, args: Vec<Identifier>, body: AST) -> AST {
        todo!()
    }

    fn call_function(&mut self, name: Identifier, args: Vec<AST>) -> AST {
        todo!()
    }

    fn print(&mut self, format: String, arguments: Vec<AST>) -> AST {
        todo!()
    }

    fn block(&mut self, statements: Vec<AST>) -> AST {
        todo!()
    }

    fn top(&mut self, statements: Vec<AST>) -> AST {
        todo!()
    }

    fn loop_de_loop(&mut self, condition: AST, body: AST) -> AST {
        todo!()
    }

    fn conditional(&mut self, condition: AST, consequent: AST, alternate: AST) -> AST {
        todo!()
    }
}

fn main() -> Result<()> {
    // load json from stdin
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input)?;
    let ast: AST = serde_json::from_str(&input)?;
    let mut interpreter = Interpreter::new();
    interpreter.fold(ast);
    Ok(())
}
