use std::fmt::Debug;
use std::cmp::PartialEq;
use serde::{Serialize, Deserialize};

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
