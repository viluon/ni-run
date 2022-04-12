use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::io::Write;

use anyhow::*;

use fml_bc_compiler::ast::*;
use fml_bc_compiler::bc::*;
use fml_bc_compiler::util::BooleanAssertions;
use itertools::Either;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Constant {
    Null,
    Boolean(bool),
    Integer(i32),
    String(String),
    Slot(u16),
    Method { name_idx: u16, n_locals: u16, n_args: u8, code: Vec<Instr> },
}

#[derive(Debug)]
struct Prototype(
    String,
    Vec<String>,
    Box<AST>,
);

struct Compiler {
    function_name: String,
    constant_pool: Vec<Constant>,
    entry_point: Option<u16>,
    functions: VecDeque<Prototype>,
    n_locals: u16,
    scopes: Vec<BTreeMap<String, u16>>,
    code: Vec<Instr>,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            function_name: "main".to_string(),
            constant_pool: vec![],
            entry_point: None,
            functions: Default::default(),
            n_locals: 0,
            scopes: vec![],
            code: vec![],
        }
    }

    fn finalise_function(&mut self, n_args: u8) -> Result<u16> {
        let name_idx = self.fetch_or_add_constant(Constant::String(self.function_name.clone()));
        let method_idx = self.constant_pool.len() as u16;
        self.constant_pool.push(Constant::Method {
            name_idx,
            n_args,
            n_locals: self.n_locals,
            code: self.code.drain(..).collect(),
        });
        self.scopes[0].insert(self.function_name.clone(), method_idx);

        self.n_locals = 0;
        Ok(self.constant_pool.len() as u16 - 1)
    }

    fn assemble(mut self) -> Result<Vec<u8>> {
        use fml_bc_compiler::bc::Constant as K;
        let mut pool = vec![];
        let mut code = vec![];

        for k in self.constant_pool.drain(..) {
            pool.push(match k {
                Constant::Null => K::Null,
                Constant::Boolean(b) => K::Boolean(b),
                Constant::Integer(i) => K::Integer(i),
                Constant::String(s) => K::String(s),
                Constant::Slot(j) => K::Slot(j),
                Constant::Method {
                    name_idx, n_locals, n_args, code: method_code
                } => {
                    let start = code.len();
                    let length = method_code.len();
                    code.extend(method_code);
                    K::Method {
                        name_idx, n_args, n_locals, start, length,
                    }
                },
            })
        }

        let mut bytecode = vec![];
        bytecode.extend(serialise(pool, code)?);
        // globals
        // TODO: methods
        bytecode.extend((self.scopes[0].len() as u16).to_le_bytes());
        for &idx in self.scopes[0].values() {
            bytecode.extend(idx.to_le_bytes());
        }
        // entry point
        bytecode.extend(self.entry_point.unwrap().to_le_bytes());

        Ok(bytecode)
    }

    fn fetch_or_add_constant(&mut self, k: Constant) -> u16 {
        self.constant_pool.iter().position(|c| c == &k).unwrap_or_else(|| {
            self.constant_pool.push(k);
            self.constant_pool.len() - 1
        }) as u16
    }

    fn find_variable<'a>(&self, name: &'a str) -> Result<Either<&'a str, u16>> {
        for (lvl, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(&idx) = scope.get(name) {
                return Ok(if lvl == 0 {
                    Either::Left(name) // global scope
                } else {
                    Either::Right(idx)
                });
            }
        }
        Err(anyhow!("no such variable: {}", name))
    }

    fn introduce_local(&mut self, name: String) -> Result<u16> {
        let n = self.n_locals;
        (self.scopes.len() > 1).expect(|| anyhow!("local variable in global scope"))?;
        self.scopes.last_mut().unwrap().insert(name, n);
        self.n_locals += 1;
        Ok(n)
    }

    fn get_or_introduce_global(&mut self, name: String) -> u16 {
        let k = self.fetch_or_add_constant(Constant::String(name.clone()));
        self.scopes[0].get(&name).map(|_| ()).unwrap_or_else(|| {
            let slot = self.fetch_or_add_constant(Constant::Slot(k));
            self.scopes[0].insert(name, slot);
        });
        k
    }

    fn emit(&mut self, instr: Instr) -> Result<()> {
        self.code.push(instr);
        Ok(())
    }

    fn compile_node(&mut self, node: &AST) -> Result<()> {
        use Instr::*;

        match node {
            AST::Integer(n) => {
                let k = self.fetch_or_add_constant(Constant::Integer(*n));
                self.emit(Literal(k))?;
            },
            AST::Boolean(b) => {
                let k = self.fetch_or_add_constant(Constant::Boolean(*b));
                self.emit(Literal(k))?;
            },
            AST::Null => {
                let k = self.fetch_or_add_constant(Constant::Null);
                self.emit(Literal(k))?;
            },
            AST::Variable { name, value } => {
                self.compile_node(value)?;
                if self.scopes.len() == 1 {
                    let var = self.get_or_introduce_global(name.0.clone());
                    self.emit(SetGlobal(var))?;
                } else {
                    let var = self.introduce_local(name.0.clone())?;
                    self.emit(SetLocal(var))?;
                }
            },
            AST::Array { size, value } => todo!(),
            AST::Object { extends, members } => todo!(),
            AST::AccessVariable { name } => {
                let instr = self.find_variable(&name.0)
                    .map(|v| match v {
                        Either::Left(_global) => GetGlobal(self.get_or_introduce_global(name.0.clone())),
                        Either::Right(i) => GetLocal(i),
                    })
                    .unwrap_or_else(|_| {
                        GetGlobal(self.get_or_introduce_global(name.0.clone()))
                    });
                self.emit(instr)?;
            },
            AST::AccessField { object, field } => todo!(),
            AST::AccessArray { array, index } => todo!(),
            AST::AssignVariable { name, value } => {
                self.compile_node(value)?;
                let instr = self.find_variable(&name.0)
                    .map(|v| match v {
                        Either::Left(_global) => SetGlobal(self.get_or_introduce_global(name.0.clone())),
                        Either::Right(i) => SetLocal(i),
                    })
                    .unwrap_or_else(|_| {
                        SetGlobal(self.get_or_introduce_global(name.0.clone()))
                    });
                self.emit(instr)?;
            },
            AST::AssignField { object, field, value } => todo!(),
            AST::AssignArray { array, index, value } => todo!(),
            AST::Function { name, parameters, body } =>
                self.functions.push_back(Prototype(
                    name.0.clone(),
                    parameters.iter().map(|p| p.0.clone()).collect(),
                    body.clone(),
                )),
            AST::CallFunction { name, arguments } => {
                let k = self.fetch_or_add_constant(Constant::String(name.0.clone()));
                for arg in arguments {
                    self.compile_node(arg)?;
                }
                self.emit(CallFunction(k, arguments.len() as u8))?;
            },
            AST::CallMethod { object, name, arguments } => todo!(),
            AST::Top(stmts) => {
                // TODO maybe we need something more here?
                for stmt in stmts {
                    self.compile_node(stmt)?;
                }
            },
            AST::Block(stmts) => {
                self.scopes.push(Default::default());
                for (i, stmt) in stmts.iter().enumerate() {
                    self.compile_node(stmt)?;
                    if i < stmts.len() - 1 {
                        self.emit(Drop)?;
                    }
                }
                self.scopes.pop();
            },
            AST::Loop { condition, body } => todo!(),
            AST::Conditional { condition, consequent, alternative } => todo!(),
            AST::Print { format, arguments } => {
                let k = self.fetch_or_add_constant(Constant::String(format.clone()));
                for arg in arguments {
                    self.compile_node(arg)?;
                }
                self.emit(Print(k, arguments.len() as u8))?;
            },
        }

        Ok(())
    }

    fn compile(&mut self, program: &AST) -> Result<()> {
        self.functions.push_back(Prototype(
            "main".to_string(),
            vec![],
            Box::new(program.clone()),
        ));

        while let Some(
            Prototype(name, params, body)
        ) = self.functions.pop_front() {
            let n_args = params.len() as u8;
            self.function_name = name;
            if self.scopes.len() > 1 {
                self.scopes.drain(1..);
            }
            // add a new scope and add all parameters to it
            self.scopes.push(Default::default());
            for p in params.into_iter() {
                self.introduce_local(p.clone()).map_err(|_| anyhow!("could not introduce {}", p.clone()))?;
            }

            self.compile_node(&body)?;
            let pos = self.finalise_function(n_args)?;
            // the first compiled function is the entry point
            self.entry_point.get_or_insert(pos);
        }

        Ok(())
    }
}

fn main() -> Result<()> {
    let ast = serde_json::from_reader(std::io::stdin())?;
    let mut c = Compiler::new();
    c.compile(&ast)?;
    let buf = c.assemble()?;
    let amount = std::io::stdout().write(&buf)?;

    if amount != buf.len() {
        panic!("could not write all bytes");
    }

    Ok(())
}
