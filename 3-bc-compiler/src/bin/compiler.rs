use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::io::Write;

use anyhow::*;

use fml_bc_compiler::ast::*;
use fml_bc_compiler::bc::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Constant {
    Null,
    Boolean(bool),
    Integer(i32),
    String(String),
    Slot(u16),
    Method { name_idx: u16, n_locals: u16, n_args: u8, code: Vec<Instr> },
}

struct Prototype(String, Vec<String>, Box<AST>);

struct Compiler {
    function_name: String,
    constant_pool: Vec<Constant>,
    entry_point: Option<u16>,
    functions: VecDeque<Prototype>,
    n_locals: u16,
    scopes: Vec<BTreeMap<String, u16>>,
    globals: Vec<(String, u16)>,
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
            globals: vec![],
            code: vec![],
        }
    }

    fn finalise_function(&mut self, n_args: u8) -> Result<u16> {
        let name_idx = self.fetch_or_add_constant(Constant::String(self.function_name.clone()));
        self.constant_pool.push(Constant::Method {
            name_idx,
            n_args,
            n_locals: self.n_locals,
            code: self.code.drain(..).collect(),
        });

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
        bytecode.extend((self.globals.len() as u16).to_le_bytes());
        for (_, idx) in self.globals.drain(..) {
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

    fn find_local(&self, name: &str) -> Option<u16> {
        for scope in self.scopes.iter().rev() {
            if let Some(&idx) = scope.get(name) {
                return Some(idx as u16);
            }
        }
        None
    }

    fn introduce_local(&mut self, name: String) -> u16 {
        let n = self.n_locals;
        self.scopes.last_mut().unwrap().insert(name, n);
        self.n_locals += 1;
        n
    }

    fn get_or_introduce_global(&mut self, name: String) -> u16 {
        let k = self.fetch_or_add_constant(Constant::String(name.clone()));
        self.globals.iter().find(|(n, _)| n == &name).map(|_| ()).unwrap_or_else(|| {
            let slot = self.fetch_or_add_constant(Constant::Slot(k));
            self.globals.push((name, slot));
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
                let var = self.introduce_local(name.0.clone());
                self.emit(SetLocal(var))?;
            },
            AST::Array { size, value } => todo!(),
            AST::Object { extends, members } => todo!(),
            AST::AccessVariable { name } => {
                let instr = self.find_local(&name.0)
                    .map(|i| GetLocal(i as u16))
                    .unwrap_or_else(|| {
                        let k = self.get_or_introduce_global(name.0.clone());
                        GetGlobal(k)
                    });
                self.emit(instr)?;
            },
            AST::AccessField { object, field } => todo!(),
            AST::AccessArray { array, index } => todo!(),
            AST::AssignVariable { name, value } => {
                self.compile_node(value)?;
                let instr = self.find_local(&name.0)
                    .map(|i| SetLocal(i as u16))
                    .unwrap_or_else(|| {
                        let k = self.get_or_introduce_global(name.0.clone());
                        SetGlobal(k)
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
            self.scopes.push(BTreeMap::from_iter(params
                .into_iter()
                .enumerate()
                .map(|(i, p)| (p, i as u16))
            ));
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
