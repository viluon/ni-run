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
    Class(Vec<u16>),
}

#[derive(Debug, Clone)]
struct Prototype {
    is_method: bool, // true if the prototype is a method (takes a receiver)
    k: u16, // index into the constant pool
         // at which the method will be stored
         // after compilation
    name: String,
    params: Vec<String>,
    body: Box<AST>,
}

struct Compiler {
    current_proto: Prototype,
    constant_pool: Vec<Constant>,
    entry_point: Option<u16>,
    functions: VecDeque<Prototype>,
    n_locals: u16,
    n_labels: u16,
    scopes: Vec<BTreeMap<String, u16>>,
    code: Vec<Instr>,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            current_proto: Prototype {
                is_method: false,
                k: 0,
                name: String::new(),
                params: vec![],
                body: Box::new(AST::Null)
            },
            constant_pool: vec![],
            entry_point: None,
            functions: Default::default(),
            n_locals: 0,
            n_labels: 0,
            scopes: vec![],
            code: vec![],
        }
    }

    fn schedule_function(&mut self, name: String, params: Vec<String>, body: AST) -> u16 {
        self.schedule_prototype(name, params, body, false)
    }

    fn schedule_method(&mut self, name: String, params: Vec<String>, body: AST) -> u16 {
        self.schedule_prototype(name, params, body, true)
    }

    fn schedule_prototype(&mut self, name: String, params: Vec<String>, body: AST, is_method: bool) -> u16 {
        let k = self.constant_pool.len() as u16;
        // bogus values to avoid deduplication choosing this index
        self.constant_pool.push(Constant::Method { name_idx: u16::MAX, n_locals: u16::MAX, n_args: u8::MAX, code: vec![] });
        self.functions.push_back(Prototype { is_method, k, name, params, body: Box::new(body) });
        k
    }

    fn finalise_function(&mut self, n_args: u8) -> Result<u16> {
        let name_idx = self.fetch_or_add_constant(Constant::String(self.current_proto.name.clone()));
        let pos = self.current_proto.k;
        self.constant_pool[pos as usize] = Constant::Method {
            name_idx,
            n_args,
            n_locals: self.n_locals,
            code: self.code.drain(..).collect(),
        };
        self.scopes[0].insert(self.current_proto.name.clone(), pos);

        self.n_locals = 0;
        Ok(pos)
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
                Constant::Class(member_indices) => K::Class { member_indices },
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
            AST::Array { size: _, value: _ } => todo!(),
            AST::Object { extends, members } => {
                self.compile_node(extends)?;
                let mut fields = vec![];
                for member in members {
                    match member {
                        AST::Variable { name, value } => {
                            self.compile_node(value)?;
                            let name_str = self.fetch_or_add_constant(Constant::String(name.0.clone()));
                            // add a slot
                            fields.push(self.fetch_or_add_constant(Constant::Slot(name_str)));
                        },
                        AST::Function { name, parameters, body } => {
                            fields.push(self.schedule_method(
                                name.0.clone(),
                                parameters.iter().map(|p| p.0.clone()).collect(),
                                *body.clone(),
                            ));
                        },
                        _ => return Err(anyhow!("unexpected node in object: {:?}", member)),
                    }
                }
                let class = self.fetch_or_add_constant(Constant::Class(fields));
                self.emit(Object(class))?;
            },
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
            AST::AccessField { object, field } => {
                self.compile_node(object)?;
                let field_str = self.fetch_or_add_constant(Constant::String(field.0.clone()));
                self.emit(GetField(field_str))?;
            },
            AST::AccessArray { array: _, index: _ } => todo!(),
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
            AST::AssignField { object, field, value } => {
                self.compile_node(object)?;
                self.compile_node(value)?;
                let field_str = self.fetch_or_add_constant(Constant::String(field.0.clone()));
                self.emit(SetField(field_str))?;
            },
            AST::AssignArray { array: _, index: _, value: _ } => todo!(),
            AST::Function { name, parameters, body } => {
                self.schedule_function(
                    name.0.clone(),
                    parameters.iter().map(|p| p.0.clone()).collect(),
                    *body.clone(),
                );
            },
            AST::CallFunction { name, arguments } => {
                let k = self.fetch_or_add_constant(Constant::String(name.0.clone()));
                for arg in arguments {
                    self.compile_node(arg)?;
                }
                self.emit(CallFunction(k, arguments.len() as u8))?;
            },
            AST::CallMethod { object, name, arguments } => {
                self.compile_node(object)?;
                let k = self.fetch_or_add_constant(Constant::String(name.0.clone()));
                for arg in arguments {
                    self.compile_node(arg)?;
                }
                self.emit(CallMethod(k, arguments.len() as u8 + 1))?;
            },
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
            AST::Loop { condition, body } => {
                let cond_lbl = self.new_label("loop:cond");
                let body_lbl = self.new_label("loop:body");
                self.emit(Jump(cond_lbl))?;
                self.emit(Label(body_lbl))?;
                self.compile_node(body)?;
                self.emit(Label(cond_lbl))?;
                self.compile_node(condition)?;
                self.emit(Branch(body_lbl))?;
            },
            AST::Conditional { condition, consequent, alternative } => {
                let con_lbl = self.new_label("cond:con");
                let alt_lbl = self.new_label("cond:alt");
                let end_lbl = self.new_label("cond:end");
                self.compile_node(condition)?;
                self.emit(Branch(con_lbl))?;
                self.emit(Label(alt_lbl))?;
                self.compile_node(alternative)?;
                self.emit(Jump(end_lbl))?;
                self.emit(Label(con_lbl))?;
                self.compile_node(consequent)?;
                self.emit(Label(end_lbl))?;
            },
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
        self.schedule_function(
            "main".to_string(),
            vec![],
            program.clone(),
        );

        while let Some(ref proto@Prototype {
            is_method,
            k: _,
            name: _,
            ref params,
            ref body
        }) = self.functions.pop_front() {
            self.current_proto = proto.clone();
            if self.scopes.len() > 1 {
                self.scopes.drain(1..);
            }
            // add a new scope and add all parameters to it
            self.scopes.push(Default::default());

            if is_method {
                self.introduce_local("this".into())?;
            }

            for p in params.iter() {
                self.introduce_local(p.clone()).map_err(|_| anyhow!("could not introduce {}", p.clone()))?;
            }

            self.compile_node(body)?;
            let n_args = params.len() as u8 + is_method as u8;
            let pos = self.finalise_function(n_args)?;
            // the first compiled function is the entry point
            self.entry_point.get_or_insert(pos);
        }

        Ok(())
    }

    fn new_label(&mut self, name_hint: &str) -> u16 {
        let name = format!("{}:{}:{}", self.n_labels, self.current_proto.name, name_hint);
        self.n_labels += 1;
        self.fetch_or_add_constant(Constant::String(name))
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
