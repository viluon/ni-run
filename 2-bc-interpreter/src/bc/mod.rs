use anyhow::Result;
use anyhow::anyhow;
use itertools::Itertools;

use crate::util::*;

/*
0x00	Label
0x01	Literal
0x02	Print
0x03	Array
0x04	Object
0x05	Get field
0x06	Set field
0x07	Call method
0x08	Call function
0x09	Set local
0x0A	Get local
0x0B	Set global
0x0C	Get global
0x0D	Branch
0x0E	Jump
0x0F	Return
0x10	Drop
*/

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Opcode {
    Literal,
    Drop,
    Print,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instr {
    Literal(u16),
    Drop,
    Print(u16, u8),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constant {
    Null,
    Boolean(bool),
    Integer(i32),
    String(String),
    Slot(u16),
    Method { name_idx: u16, n_args: u8, n_locals: u16, start: usize, length: usize },
}

impl Constant {
    pub fn as_string(&self) -> Result<String> {
        match self {
            Constant::String(s) => Ok(s.clone()),
            k => Err(anyhow!("expected string, got {:?}", k))
        }
    }
}

#[derive(Debug, Clone)]
enum Layout {
    Nullary,
    Constant(u16),
    ConstantAndByte(u16, u8),
}

type BcInstr = (Opcode, Layout);

fn parse_code<'a, It: Iterator<Item = &'a u8>>(iter: &mut It, len: u32) -> Result<Vec<Instr>> {
    (0..len).map(|_| parse_instr(iter)?.ok_or_else(eos)).collect()
}

pub fn parse_constant_pool<'a, It: Iterator<Item = &'a u8>>(bc: &mut It) -> Result<(Vec<Constant>, Vec<Instr>)> {
    let len = next_u16(bc)?;
    let mut constants = vec![];
    let mut code = vec![];
    for _ in 0..len {
        let &tag = bc.next().ok_or_else(eos)?;
        let constant = match tag {
            0x01 => Constant::Null,
            0x06 => Constant::Boolean(bc.next().ok_or_else(eos)? == &0x01),
            0x00 => Constant::Integer(next_i32(bc)?),
            0x02 => {
                let length = next_u32(bc)?;
                let str = String::from_utf8(bc.take(length as usize).cloned().collect())?;
                let actual_length = str.as_bytes().len();
                (actual_length == length as usize).expect(|| anyhow!("expected {} bytes, got {}", length, actual_length))?;
                Constant::String(str)
            },
            0x04 => Constant::Slot(next_u16(bc)?),
            0x03 => {
                let name_idx = next_u16(bc)?;
                let n_args = *bc.next().ok_or_else(eos)?;
                let n_locals = next_u16(bc)?;
                let n_instr = next_u32(bc)?;
                let start = code.len();
                code.extend(parse_code(bc, n_instr)?);
                let length = code.len() - start;

                Constant::Method { name_idx, n_args, n_locals, start, length }
            }
            _ => return Err(anyhow!("Invalid constant tag: {}", tag)),
        };
        constants.push(constant);
    }

    Ok((constants, code))
}

pub fn parse_globals<'a, It: Iterator<Item = &'a u8>>(iter: &mut It) -> Result<Vec<u16>> {
    let len = next_u16(iter)?;
    (0..len).map(|_| next_u16(iter)).collect()
}

pub fn parse_entry_point<'a, It: Iterator<Item = &'a u8>>(iter: &mut It) -> Result<u16> {
    next_u16(iter)
}

fn next_u16<'a, It: Iterator<Item = &'a u8>>(iter: &mut It) -> Result<u16, anyhow::Error> {
    Ok(u16::from_le_bytes([*iter.next().ok_or_else(eos)?, *iter.next().ok_or_else(eos)?]))
}

fn next_u32<'a, It: Iterator<Item = &'a u8>>(iter: &mut It) -> Result<u32, anyhow::Error> {
    let (&b1, &b2, &b3, &b4) = iter.next_tuple().ok_or_else(eos)?;
    Ok(u32::from_le_bytes([b1, b2, b3, b4]))
}

fn next_i32<'a, It: Iterator<Item = &'a u8>>(iter: &mut It) -> Result<i32, anyhow::Error> {
    let (&b1, &b2, &b3, &b4) = iter.next_tuple().ok_or_else(eos)?;
    Ok(i32::from_le_bytes([b1, b2, b3, b4]))
}

fn parse_bc_instr<'a, It: Iterator<Item = &'a u8>>(bc: &mut It) -> Result<Option<BcInstr>> {
    if let Some(&opcode) = bc.next() {
        let opcode = Opcode::try_from(opcode)?;
        let kind = match opcode {
            Opcode::Literal => Layout::constant(bc)?,
            Opcode::Drop    => Layout::nullary(),
            Opcode::Print   => Layout::constant_and_byte(bc)?,
        };
        Ok(Some((opcode, kind)))
    } else {
        Ok(None)
    }
}

fn parse_instr<'a, It: Iterator<Item = &'a u8>>(bc: &mut It) -> Result<Option<Instr>> {
    if let Some(bc_instr) = parse_bc_instr(bc)? {
        Ok(Some(bc_instr.try_into()?))
    } else { Ok(None) }
}

fn eos() -> anyhow::Error {
    anyhow!("end of stream")
}

impl TryFrom<u8> for Opcode {
    type Error = anyhow::Error;

    fn try_from(value: u8) -> Result<Self> {
        match value {
            0x01 => Ok(Opcode::Literal),
            0x10 => Ok(Opcode::Drop),
            0x02 => Ok(Opcode::Print),
            _ => Err(anyhow!("invalid opcode: {}", value)),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(opcode: Opcode) -> Self {
        match opcode {
            Opcode::Literal => 0x01,
            Opcode::Drop => 0x10,
            Opcode::Print => 0x02,
        }
    }
}

impl Layout {
    fn nullary() -> Self {
        Layout::Nullary
    }

    fn constant<'a, It: Iterator<Item = &'a u8>>(bc: &mut It) -> Result<Layout> {
        Ok(Layout::Constant(next_u16(bc)?))
    }

    fn constant_and_byte<'a, It: Iterator<Item = &'a u8>>(bc: &mut It) -> Result<Layout> {
        let k = next_u16(bc)?;
        let b = bc.next().ok_or_else(eos)?;
        Ok(Layout::ConstantAndByte(k, *b))
    }
}

impl TryFrom<BcInstr> for Instr {
    type Error = anyhow::Error;

    fn try_from(bc_instr: BcInstr) -> Result<Self> {
        match bc_instr {
            (Opcode::Literal, Layout::Constant(k)) => Ok(Instr::Literal(k)),
            (Opcode::Drop,    Layout::Nullary) => Ok(Instr::Drop),
            (Opcode::Print,   Layout::ConstantAndByte(k, b)) => Ok(Instr::Print(k, b)),
            _ => Err(anyhow!("invalid bc instr: {:?}", bc_instr)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_opcode_from_u8() {
        assert_eq!(Opcode::Literal, Opcode::try_from(0x01).unwrap());
        assert_eq!(Opcode::Drop, Opcode::try_from(0x10).unwrap());
        assert_eq!(Opcode::Print, Opcode::try_from(0x02).unwrap());
    }

    #[quickcheck]
    fn qc_opcode_from_into_inverse(v: u8) {
        if let Ok(opcode) = Opcode::try_from(v) {
            let serialised: u8 = opcode.into();
            assert_eq!(serialised, v)
        }
    }
}
