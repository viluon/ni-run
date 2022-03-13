use anyhow::Result;
use anyhow::anyhow;
use itertools::Itertools;

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
}

#[derive(Debug, Clone)]
enum Kind {
    Nullary,
    Constant(u16),
    ConstantAndByte(u16, u8),
}

type BcInstr = (Opcode, Kind);

pub fn parse_code(bc: &[u8]) -> Result<Vec<Instr>> {
    let mut iter = bc.iter();
    let mut res = Vec::new();
    while let Some(instr) = parse_instr(&mut iter)? {
        res.push(instr)
    }
    Ok(res)
}

pub fn parse_constant_pool(bc: &[u8]) -> Result<Vec<Constant>> {
    let mut iter = bc.iter();
    let mut res = Vec::new();
    while let Some(constant) = parse_constant(&mut iter)? {
        res.push(constant)
    }
    Ok(res)
}

fn parse_bc_instr<'a, It: Iterator<Item = &'a u8>>(bc: &mut It) -> Result<Option<BcInstr>> {
    if let Some(&opcode) = bc.next() {
        let opcode = Opcode::try_from(opcode)?;
        let kind = match opcode {
            Opcode::Literal => Kind::constant(bc)?,
            Opcode::Drop    => Kind::nullary(),
            Opcode::Print   => Kind::constant_and_byte(bc)?,
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

fn parse_constant<'a, It: Iterator<Item = &'a u8>>(bc: &mut It) -> Result<Option<Constant>> {
    if let Some(&tag) = bc.next() {
        let tag = tag.try_into()?;
        let constant = match tag {
            0x01 => Constant::Null,
            0x06 => Constant::Boolean(bc.next().ok_or_else(eos)? == &0x01),
            0x00 => {
                let (b1, b2, b3, b4, b5) = bc.next_tuple().ok_or_else(eos)?;
                Constant::Integer(unimplemented!())
            },
            0x03 => Constant::String(unimplemented!()),
            _    => return Err(anyhow!("Invalid constant tag: {}", tag)),
        };
        Ok(Some(constant))
    } else {
        Ok(None)
    }
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

impl Kind {
    fn nullary() -> Self {
        Kind::Nullary
    }

    fn constant<'a, It: Iterator<Item = &'a u8>>(bc: &mut It) -> Result<Kind> {
        let k = bc.next().ok_or_else(eos)?;
        let k = (*k as u16) << 8;
        let k = k | *bc.next().ok_or_else(eos)? as u16;
        Ok(Kind::Constant(k))
    }

    fn constant_and_byte<'a, It: Iterator<Item = &'a u8>>(bc: &mut It) -> Result<Kind> {
        let k = bc.next().ok_or_else(eos)?;
        let k = (*k as u16) << 8;
        let k = k | *bc.next().ok_or_else(eos)? as u16;
        let b = bc.next().ok_or_else(eos)?;
        Ok(Kind::ConstantAndByte(k, *b))
    }
}

impl TryFrom<BcInstr> for Instr {
    type Error = anyhow::Error;

    fn try_from(bc_instr: BcInstr) -> Result<Self> {
        match bc_instr {
            (Opcode::Literal, Kind::Constant(k)) => Ok(Instr::Literal(k)),
            (Opcode::Drop,    Kind::Nullary) => Ok(Instr::Drop),
            (Opcode::Print,   Kind::ConstantAndByte(k, b)) => Ok(Instr::Print(k, b)),
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
