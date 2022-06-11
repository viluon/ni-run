
use std::collections::HashMap;

use anyhow::{anyhow, Result};
use assembler::*;
use assembler::mnemonic_parameter_types::{registers::*, immediates::*, Label};
use itertools::Itertools;

use crate::bc::{Instr, Constant};
use crate::heap::Value;
use crate::interpreter::Interpreter;

const CHUNK_LENGTH: usize = 4096;
const LABEL_COUNT: usize = 64;

pub static mut INTERPRETER: Option<*mut Interpreter> = None;

pub struct CompiledChunk(ExecutableAnonymousMemoryMap, pub unsafe extern "C" fn() -> i64);

impl CompiledChunk {
    fn assemble<F>(mut code: F) -> Result<CompiledChunk>
    where F: FnMut(&mut InstructionStream) {
        let mut memory_map = ExecutableAnonymousMemoryMap::new(CHUNK_LENGTH, true, true)?;
        let f = {
            let mut instr_stream = memory_map.instruction_stream(&InstructionStreamHints {
                number_of_labels: LABEL_COUNT,
                ..Default::default()
            });

            let f = instr_stream.nullary_function_pointer::<i64>();
            code(&mut instr_stream);
            instr_stream.finish();
            f
        };
        Ok(CompiledChunk(memory_map, f))
    }
}

fn interpreter() -> &'static mut Interpreter {
    unsafe { INTERPRETER.unwrap().as_mut().unwrap() }
}

// runtime functions
unsafe extern "C" fn gc_check() {
    let i = interpreter();
    if i.should_gc {
        i.gc().unwrap();
        i.should_gc = false;
    }
}

unsafe extern "C" fn perform_return() {
    interpreter().perform_return().unwrap()
}

unsafe extern "C" fn truthy() -> i64 {
    interpreter().peek().unwrap().is_truthy() as i64
}

unsafe extern "C" fn create_array() {
    interpreter().create_array().unwrap()
}

unsafe extern "C" fn create_object(k: i64) {
    interpreter().create_object(k as u16).unwrap()
}

unsafe extern "C" fn perform_print(k: i64, n_args: i64) {
    interpreter().perform_print(k as u16, n_args as u8).unwrap()
}

unsafe extern "C" fn get_local(k: i64) {
    interpreter().get_local(k as u16).unwrap()
}

unsafe extern "C" fn set_local(k: i64) {
    interpreter().set_local(k as u16).unwrap()
}

unsafe extern "C" fn get_global(name: i64) {
    interpreter().get_global(name as u16).unwrap()
}

unsafe extern "C" fn set_global(name: i64) {
    interpreter().set_global(name as u16).unwrap()
}

unsafe extern "C" fn get_field(name: i64) {
    interpreter().get_field(name as u16).unwrap()
}

unsafe extern "C" fn set_field(name: i64) {
    interpreter().set_field(name as u16).unwrap()
}

unsafe extern "C" fn call_method(k: i64, n_args: i64) {
    interpreter().perform_method_call(k as u16, n_args as u8).unwrap()
}

unsafe extern "C" fn call_function(k: i64, n_args: i64) {
    let global_name = interpreter().constant_pool[k as usize].as_str().unwrap();
    let fn_addr = interpreter().find_global_function(global_name, k as u16).unwrap();
    interpreter().perform_function_call(fn_addr, n_args as u8).unwrap()
}

struct JitCompiler<'a> {
    instr_stream: InstructionStream<'a>,
    label_map: HashMap<usize, Label>,
}

fn get_constant(k: u16) -> Result<&'static Constant> {
    interpreter().constant_pool.get(k as usize)
        .ok_or_else(|| anyhow!("invalid constant index"))
}

impl<'a> JitCompiler<'a> {
    fn emit_code_for(&mut self, instr: &Instr) -> Result<()> {
        match instr {
            &Instr::Literal(k) => self.emit_push(get_constant(k)?.as_value()?),
            Instr::LiteralNull => self.emit_push(Value::Null),
            Instr::LiteralBool(b) => self.emit_push(Value::Bool(*b)),
            Instr::LiteralInt(i) => self.emit_push(Value::Int(*i)),
            Instr::Drop => self.emit_pop(Register64Bit::RAX),
            &Instr::Print(k, n_args) => self.emit_call2(perform_print, k as i64, n_args as i64),
            Instr::GetLocal(i) => self.emit_call1(get_local, *i as i64),
            Instr::SetLocal(i) => self.emit_call1(set_local, *i as i64),
            Instr::GetGlobal(i) |
            Instr::GetGlobalDirect(i) => self.emit_call1(get_global, *i as i64),
            Instr::SetGlobal(i) |
            Instr::SetGlobalDirect(i) => self.emit_call1(set_global, *i as i64),
            Instr::GetField(i) |
            Instr::GetFieldDirect(i) => self.emit_call1(get_field, *i as i64),
            Instr::SetField(i) |
            Instr::SetFieldDirect(i) => self.emit_call1(set_field, *i as i64),
            Instr::Label(_) => unreachable!("labels should be collected by the label collection pass"),
            Instr::Jump(_) => todo!(),
            Instr::JumpDirect(target) => self.instr_stream
                .jmp_Label(self.label_map[target])
                .map_err(|_| anyhow!("{}? I can't jump that far!", target)),
            Instr::Branch(_) => todo!(),
            Instr::BranchDirect(target) => {
                self.emit_call0r(truthy)?;
                self.instr_stream.mov_Register64Bit_Register64Bit_r64_rm64(Register64Bit::R12, Register64Bit::RAX);
                self.emit_pop(Register64Bit::RAX)?;
                self.instr_stream.cmp_Register64Bit_Immediate32Bit(Register64Bit::R12, 0_u32.into());
                self.instr_stream
                    .jne_Label(self.label_map[target])
                    .map_err(|_| anyhow!("{}? I can't jump that far!", target))
            },
            &Instr::CallMethod(k, n_args) => self.emit_call2(call_method, k as i64, n_args as i64),
            &Instr::CallFunction(k, n_args) => self.emit_call2(call_function, k as i64, n_args as i64),
            Instr::Return => self.emit_call0(perform_return),
            Instr::Array => self.emit_call0(create_array),
            Instr::Object(i) => self.emit_call1(create_object, *i as i64),
        }
    }

    fn emit_pop(&mut self, reg: Register64Bit) -> Result<()> {
        todo!()
    }

    fn emit_push(&mut self, v: Value) -> Result<()> {
        todo!()
    }

    fn emit_call0(&mut self, f: unsafe extern "C" fn()) -> Result<()> {
        self.instr_stream.push_Register64Bit_r64(Register64Bit::RAX);
        self.instr_stream.mov_Register64Bit_Immediate64Bit(Register64Bit::RAX, (f as usize as i64).into());
        self.instr_stream.call_Register64Bit(Register64Bit::RAX);
        self.instr_stream.pop_Register64Bit_r64(Register64Bit::RAX);
        Ok(())
    }

    fn emit_call0r(&mut self, f: unsafe extern "C" fn() -> i64) -> Result<()> {
        self.instr_stream.push_Register64Bit_r64(Register64Bit::RAX);
        self.instr_stream.mov_Register64Bit_Immediate64Bit(Register64Bit::RAX, (f as usize as i64).into());
        self.instr_stream.call_Register64Bit(Register64Bit::RAX);
        self.instr_stream.pop_Register64Bit_r64(Register64Bit::RDI);
        Ok(())
    }

    fn emit_call1(&mut self, f: unsafe extern "C" fn(i64), a: i64) -> Result<()> {
        self.instr_stream.push_Register64Bit_r64(Register64Bit::RAX);
        self.instr_stream.mov_Register64Bit_Immediate64Bit(Register64Bit::RDI, a.into());
        self.instr_stream.mov_Register64Bit_Immediate64Bit(Register64Bit::RAX, (f as usize as i64).into());
        self.instr_stream.call_Register64Bit(Register64Bit::RAX);
        self.instr_stream.pop_Register64Bit_r64(Register64Bit::RAX);
        Ok(())
    }

    fn emit_call2(&mut self, f: unsafe extern "C" fn(i64, i64), a: i64, b: i64) -> Result<()> {
        self.instr_stream.push_Register64Bit_r64(Register64Bit::RAX);
        self.instr_stream.mov_Register64Bit_Immediate64Bit(Register64Bit::RDI, a.into());
        self.instr_stream.mov_Register64Bit_Immediate64Bit(Register64Bit::RSI, b.into());
        self.instr_stream.mov_Register64Bit_Immediate64Bit(Register64Bit::RAX, (f as usize as i64).into());
        self.instr_stream.call_Register64Bit(Register64Bit::RAX);
        self.instr_stream.pop_Register64Bit_r64(Register64Bit::RAX);
        Ok(())
    }

    fn assemble(&mut self) -> Result<CompiledChunk> {
        todo!()
    }
}

pub fn compile(offset: usize, code: &[Instr]) -> Result<CompiledChunk> {
    let mut mmap = ExecutableAnonymousMemoryMap::new(CHUNK_LENGTH, true, true)?;
    let mut jit = JitCompiler {
        label_map: Default::default(),
        instr_stream: mmap.instruction_stream(&InstructionStreamHints {
            number_of_labels: LABEL_COUNT,
            ..Default::default()
        })
    };

    jit.label_map = interpreter().label_map.values().copied().sorted()
        .map(|pos| (pos, jit.instr_stream.create_label())).collect();

    for (i, instr) in code.iter().enumerate() {
        let i = i + offset;
        if interpreter().label_map.values().contains(&i) {
            jit.instr_stream.attach_label(jit.label_map[&i]);
        }
        jit.emit_code_for(instr)?;
        if instr.could_alloc() {
            jit.emit_call0(gc_check)?
        }
    }
    jit.assemble()
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

    fn main() -> Result<()> {
        let chunk = CompiledChunk::assemble(|instr_stream| {
            instr_stream.mov_Register64Bit_Immediate64Bit(Register64Bit::RAX, Immediate64Bit(0x123456789abcdef0));
            instr_stream.ret();
        })?;

        assert_eq!(unsafe { chunk.1() }, 0x123456789abcdef0);
        Ok(())
    }

    #[test]
    fn test_run() -> Result<()> {
        main()
    }
}
