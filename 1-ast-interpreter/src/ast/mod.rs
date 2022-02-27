use std::fmt::Debug;
use std::cmp::PartialEq;
use serde::{Serialize, Deserialize};

pub mod fold {
    use super::*;

    pub trait AstFold {
        fn integer(
            &mut self, i: i32
        ) -> AST {
            AST::Integer(i)
        }

        fn boolean(
            &mut self, b: bool
        ) -> AST {
            AST::Boolean(b)
        }

        fn null(
            &mut self
        ) -> AST {
            AST::Null
        }

        fn variable(
            &mut self, name: Identifier, value: AST
        ) -> AST {
            AST::Variable { name, value: Box::new(self.fold(value)) }
        }

        fn array(
            &mut self, size: AST, value: AST
        ) -> AST {
            AST::Array { size: Box::new(self.fold(size)), value: Box::new(self.fold(value)) }
        }

        fn object(
            &mut self, extends: AST, members: Vec<AST>
        ) -> AST {
            let members = members.into_iter().map(|member| self.fold(member)).collect();
            AST::Object { extends: Box::new(self.fold(extends)), members }
        }

        fn access_variable(
            &mut self, name: Identifier
        ) -> AST {
            AST::AccessVariable { name }
        }

        fn access_field(
            &mut self, object: AST, field: Identifier
        ) -> AST {
            AST::AccessField { object: Box::new(self.fold(object)), field }
        }

        fn access_array(
            &mut self, array: AST, index: AST
        ) -> AST {
            AST::AccessArray { array: Box::new(self.fold(array)), index: Box::new(self.fold(index)) }
        }

        fn assign_variable(
            &mut self, name: Identifier, value: AST
        ) -> AST {
            AST::AssignVariable { name, value: Box::new(self.fold(value)) }
        }

        fn assign_field(
            &mut self, object: AST, field: Identifier, value: AST
        ) -> AST {
            AST::AssignField {
                object: Box::new(self.fold(object)),
                field,
                value: Box::new(self.fold(value))
            }
        }

        fn assign_array(
            &mut self, array: AST, index: AST, value: AST
        ) -> AST {
            AST::AssignArray {
                array: Box::new(self.fold(array)),
                index: Box::new(self.fold(index)),
                value: Box::new(self.fold(value))
            }
        }

        fn function(
            &mut self, name: Identifier, parameters: Vec<Identifier>, body: AST
        ) -> AST {
            AST::Function { name, parameters, body: Box::new(self.fold(body)) }
        }

        fn call_function(
            &mut self, name: Identifier, arguments: Vec<AST>
        ) -> AST {
            let arguments = arguments.into_iter().map(|argument| self.fold(argument)).collect();
            AST::CallFunction { name, arguments }
        }

        fn call_method(
            &mut self, object: AST, name: Identifier, arguments: Vec<AST>
        ) -> AST {
            AST::CallMethod {
                object: Box::new(self.fold(object)),
                name,
                arguments: arguments.into_iter().map(|argument| self.fold(argument)).collect()
            }
        }

        fn top(
            &mut self, statements: Vec<AST>
        ) -> AST {
            AST::Top(statements.into_iter().map(|statement| self.fold(statement)).collect())
        }

        fn block(
            &mut self, statements: Vec<AST>
        ) -> AST {
            AST::Block(statements.into_iter().map(|statement| self.fold(statement)).collect())
        }

        fn loop_de_loop(
            &mut self, condition: AST, body: AST
        ) -> AST {
            AST::Loop { condition: Box::new(self.fold(condition)), body: Box::new(self.fold(body)) }
        }

        fn conditional(
            &mut self, condition: AST, consequent: AST, alternative: AST
        ) -> AST {
            AST::Conditional {
                condition: Box::new(self.fold(condition)),
                consequent: Box::new(self.fold(consequent)),
                alternative: Box::new(self.fold(alternative))
            }
        }

        fn print(
            &mut self, format: String, arguments: Vec<AST>
        ) -> AST {
            AST::Print { format, arguments: arguments.into_iter().map(|argument| self.fold(argument)).collect() }
        }

        fn fold(
            &mut self, ast: AST
        ) -> AST {
            match ast {
                AST::Integer(n) => self.integer(n),
                AST::Boolean(b) => self.boolean(b),
                AST::Null => self.null(),
                AST::Variable { name, value } => {
                    let v = self.fold(*value);
                    self.variable(name, v)
                },
                AST::Array { size, value } => {
                    let s = self.fold(*size);
                    let v = self.fold(*value);
                    self.array(s, v)
                },
                AST::Object { extends, members } => {
                    let e = self.fold(*extends);
                    let m = members.into_iter().map(|m| self.fold(m)).collect();
                    self.object(e, m)
                },
                AST::AccessVariable { name } => self.access_variable(name),
                AST::AccessField { object, field } => {
                    let o = self.fold(*object);
                    self.access_field(o, field)
                },
                AST::AccessArray { array, index } => {
                    let a = self.fold(*array);
                    let i = self.fold(*index);
                    self.access_array(a, i)
                },
                AST::AssignVariable { name, value } => {
                    let v = self.fold(*value);
                    self.assign_variable(name, v)
                },
                AST::AssignField { object, field, value } => {
                    let o = self.fold(*object);
                    let v = self.fold(*value);
                    self.assign_field(o, field, v)
                },
                AST::AssignArray { array, index, value } => {
                    let a = self.fold(*array);
                    let i = self.fold(*index);
                    let v = self.fold(*value);
                    self.assign_array(a, i, v)
                },
                AST::Function { name, parameters, body } => {
                    let b = self.fold(*body);
                    self.function(name, parameters, b)
                },
                AST::CallFunction { name, arguments } => {
                    let a = arguments.into_iter().map(|a| self.fold(a)).collect();
                    self.call_function(name, a)
                },
                AST::CallMethod { object, name, arguments } => {
                    let o = self.fold(*object);
                    let a = arguments.into_iter().map(|a| self.fold(a)).collect();
                    self.call_method(o, name, a)
                },
                AST::Top(stmts) => {
                    let s = stmts.into_iter().map(|s| self.fold(s)).collect();
                    self.top(s)
                },
                AST::Block(stmts) => {
                    let s = stmts.into_iter().map(|s| self.fold(s)).collect();
                    self.block(s)
                },
                AST::Loop { condition, body } => {
                    let c = self.fold(*condition);
                    let b = self.fold(*body);
                    self.loop_de_loop(c, b)
                },
                AST::Conditional { condition, consequent, alternative } => {
                    let cnd = self.fold(*condition);
                    let c = self.fold(*consequent);
                    let a = self.fold(*alternative);
                    self.conditional(cnd, c, a)
                },
                AST::Print { format, arguments } => {
                    let a = arguments.into_iter().map(|a| self.fold(a)).collect();
                    self.print(format, a)
                },
            }
        }
    }
}

#[derive(PartialEq,Debug,Serialize,Deserialize,Clone)]
pub enum AST {
    Integer(i32),
    Boolean(bool),
    Null,

    Variable { name: Identifier, value: Box<AST> },
    Array { size: Box<AST>, value: Box<AST> },
    Object { extends: Box<AST>, members: Vec<AST> },

    AccessVariable { name: Identifier },
    AccessField { object: Box<AST>, field: Identifier },
    AccessArray { array: Box<AST>, index: Box<AST> },

    AssignVariable { name: Identifier, value: Box<AST> },
    AssignField { object: Box<AST>, field: Identifier, value: Box<AST> },
    AssignArray { array: Box<AST>, index: Box<AST>, value: Box<AST> },

    Function { name: Identifier, parameters: Vec<Identifier>, body: Box<AST> },

    CallFunction { name: Identifier, arguments: Vec<AST> },
    CallMethod { object: Box<AST>, name: Identifier, arguments: Vec<AST> },

    Top (Vec<AST>),
    Block (Vec<AST>),
    Loop { condition: Box<AST>, body: Box<AST> },
    Conditional { condition: Box<AST>, consequent: Box<AST>, alternative: Box<AST> },

    Print { format: String, arguments: Vec<AST> },
}

#[derive(PartialEq,Eq,Hash,PartialOrd,Ord,Debug,Clone,Serialize,Deserialize)]
pub struct Identifier(pub String);

impl From<Operator> for Identifier {
    fn from(op: Operator) -> Self {
        Identifier(op.to_string())
    }
}

#[derive(PartialEq,Debug,Copy,Clone,Serialize,Deserialize)]
pub enum Operator {
    Multiplication,
    Division,
    Module,
    Addition,
    Subtraction,
    Inequality,
    Equality,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Disjunction,
    Conjunction,
}

impl Operator {
    pub fn as_str(&self) -> &str {
        match self {
            Operator::Multiplication => "*",
            Operator::Division       => "/",
            Operator::Module         => "%",
            Operator::Addition       => "+",
            Operator::Subtraction    => "-",
            Operator::Inequality     => "!=",
            Operator::Equality       => "==",
            Operator::Less           => "<",
            Operator::LessEqual      => "<=",
            Operator::Greater        => ">",
            Operator::GreaterEqual   => ">=",
            Operator::Disjunction    => "|",
            Operator::Conjunction    => "&",
        }
    }
}

impl From<&str> for Operator {
    fn from(s: &str) -> Self {
        match s {
            "*"  => Operator::Multiplication,
            "/"  => Operator::Division,
            "%"  => Operator::Module,
            "+"  => Operator::Addition,
            "-"  => Operator::Subtraction,
            "!=" => Operator::Inequality,
            "==" => Operator::Equality,
            "<"  => Operator::Less,
            "<=" => Operator::LessEqual,
            ">"  => Operator::Greater,
            ">=" => Operator::GreaterEqual,
            "|"  => Operator::Disjunction,
            "&"  => Operator::Conjunction,

            other => panic!("Cannot parse {} as Operator", other),
        }
    }
}

impl From<String> for Operator {
    fn from(s: String) -> Self {
        Operator::from(s.as_str())
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
