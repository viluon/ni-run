use anyhow::*;

use fml_bc_interpreter::ast::*;
use fml_bc_interpreter::bc::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Constant {
    Null,
    Boolean(bool),
    Integer(i32),
    String(String),
    Slot(u16),
}

struct Compiler {
    constant_pool: Vec<Constant>,
    code: Vec<Instr>,
    variables: Vec<String>,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            constant_pool: vec![],
            code: vec![],
            variables: vec![],
        }
    }

    fn fetch_or_add_constant(&mut self, k: Constant) -> u16 {
        self.constant_pool.iter().position(|c| c == &k).unwrap_or_else(|| {
            self.constant_pool.push(k);
            self.constant_pool.len() - 1
        }) as u16
    }

    fn add_variable(&mut self, name: String) -> u16 {
        self.variables.push(name);
        self.variables.len() as u16 - 1
    }

    fn emit(&mut self, instr: Instr) -> Result<()> {
        self.code.push(instr);
        Ok(())
    }

    fn compile(&mut self, program: &AST) -> Result<()> {
        use Instr::*;

        match program {
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
                let k = self.fetch_or_add_constant(Constant::String(name.0.clone()));
                self.emit(Literal(k))?;
                self.compile(value)?;
                let var = self.add_variable(name.0.clone());
                self.emit(SetLocal(var))?;
            },
            AST::Array { size, value } => todo!(),
            AST::Object { extends, members } => todo!(),
            AST::AccessVariable { name } => {
                self.emit(GetLocal(self.variables.iter()
                    .position(|v| v == &name.0)
                    .ok_or_else(|| anyhow!("attempt to access undefined {}", name.0))? as u16)
                )?;
            },
            AST::AccessField { object, field } => todo!(),
            AST::AccessArray { array, index } => todo!(),
            AST::AssignVariable { name, value } => {
                self.compile(value)?;
                let var = self.variables.iter()
                    .position(|v| v == &name.0)
                    .ok_or_else(|| anyhow!("attempt to assign to undefined {}", name.0))?;
                self.emit(SetLocal(var as u16))?;
            },
            AST::AssignField { object, field, value } => todo!(),
            AST::AssignArray { array, index, value } => todo!(),
            AST::Function { name, parameters, body } => unimplemented!(),
            AST::CallFunction { name, arguments } => unimplemented!(),
            AST::CallMethod { object, name, arguments } => todo!(),
            AST::Top(stmts) => {
                // TODO maybe we need something more here?
                for stmt in stmts {
                    self.compile(stmt)?;
                }
            },
            AST::Block(stmts) => {
                for stmt in stmts {
                    self.compile(stmt)?;
                }
            },
            AST::Loop { condition, body } => todo!(),
            AST::Conditional { condition, consequent, alternative } => todo!(),
            AST::Print { format, arguments } => {
                let k = self.fetch_or_add_constant(Constant::String(format.clone()));
                for arg in arguments {
                    self.compile(arg)?;
                }
                self.emit(Print(k, arguments.len() as u8))?;
            },
        }

        Ok(())
    }
}

fn main() -> Result<()> {
    let ast = serde_json::from_reader(std::io::stdin())?;
    let mut c = Compiler::new();
    c.compile(&ast)?;
    println!("{:?}", c.code);
    Ok(())
}
