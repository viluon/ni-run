
use anyhow::Result;
use assembler::*;
use assembler::mnemonic_parameter_types::{registers::*, immediates::*};

const CHUNK_LENGTH: usize = 4096;
const LABEL_COUNT: usize = 64;

static mut X: u32 = 0;

unsafe extern "C" fn foo() {
    println!("hello world!");
    X += 1;
}

struct CompiledChunk(ExecutableAnonymousMemoryMap, unsafe extern "C" fn() -> i64);

impl CompiledChunk {
    fn assemble<F>(mut code: F) -> Result<CompiledChunk>
    where F: FnMut(&mut InstructionStream) {
        let mut memory_map = ExecutableAnonymousMemoryMap::new(CHUNK_LENGTH, true, true)?;
        let f = {
            let mem_ref: &mut ExecutableAnonymousMemoryMap = &mut memory_map;
            let mut instr_stream = mem_ref.instruction_stream(&InstructionStreamHints {
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

fn main() -> Result<()> {
    let chunk = CompiledChunk::assemble(|instr_stream| {
        instr_stream.push_Register64Bit_r64(Register64Bit::RAX);
        instr_stream.mov_Register64Bit_Immediate64Bit(Register64Bit::RAX, (foo as usize as i64).into());
        instr_stream.call_Register64Bit(Register64Bit::RAX);
        instr_stream.mov_Register64Bit_Immediate64Bit(Register64Bit::RAX, Immediate64Bit(0x123456789abcdef0));
        instr_stream.pop_Register64Bit_r64(Register64Bit::RCX);
        instr_stream.ret();
    })?;

    assert_eq!(unsafe { chunk.1() }, 0x123456789abcdef0);
    assert_eq!(unsafe { X }, 1);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

    #[test]
    fn test_run() -> Result<()> {
        main()
    }
}
