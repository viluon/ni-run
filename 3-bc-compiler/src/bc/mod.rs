use anyhow::Result;
use anyhow::anyhow;
use itertools::Itertools;
use std::collections::HashMap;

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
    GetLocal,
    SetLocal,
    GetGlobal,
    SetGlobal,
    GetField,
    SetField,
    Label,
    Jump,
    Branch,
    CallMethod,
    CallFunction,
    Return,
    Array,
    Object,
}

pub type Pc = usize;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instr {
    Literal(u16),
    LiteralNull,
    LiteralBool(bool),
    LiteralInt(i32),
    Drop,
    Print(u16, u8),
    GetLocal(u16),
    SetLocal(u16),
    GetGlobal(u16),
    GetGlobalDirect(u16),
    SetGlobal(u16),
    SetGlobalDirect(u16),
    GetField(u16),
    GetFieldDirect(u16),
    SetField(u16),
    SetFieldDirect(u16),
    Label(u16),
    Jump(u16),
    JumpDirect(Pc), // FIXME: bumps repr size (actually, Strings do too)
    Branch(u16),
    BranchDirect(Pc), // ditto
    CallMethod(u16, u8),
    CallFunction(u16, u8),
    Return,
    Array,
    Object(u16),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constant {
    Null,
    Boolean(bool),
    Integer(i32),
    String(String),
    Slot(u16),
    Method { name_idx: u16, n_args: u8, n_locals: u16, start: Pc, length: usize },
    Class { member_indices: Vec<u16> },
}

impl Constant {
    pub fn as_string(&self) -> Result<String> {
        match self {
            Constant::String(s) => Ok(s.clone()),
            k => Err(anyhow!("expected string, got {:?}", k))
        }
    }

    pub fn as_str(&self) -> Result<&str> {
        match self {
            Constant::String(s) => Ok(s.as_str()),
            k => Err(anyhow!("expected string, got {:?}", k))
        }
    }

    pub fn as_method(&self) -> Result<(u16, u8, u16, usize, usize)> {
        match self {
            Constant::Method { name_idx, n_args, n_locals, start, length } =>
                Ok((*name_idx, *n_args, *n_locals, *start, *length)),
            k => Err(anyhow!("expected method, got {:?}", k))
        }
    }

    pub fn as_slot(&self) -> Result<u16> {
        match self {
            Constant::Slot(s) => Ok(*s),
            k => Err(anyhow!("expected slot, got {:?}", k))
        }
    }

    pub fn as_class(&self) -> Result<Vec<u16>> {
        match self {
            Constant::Class { member_indices } => Ok(member_indices.clone()),
            k => Err(anyhow!("expected class, got {:?}", k))
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
            0x00 => Constant::Integer(next_i32(bc)?),
            0x01 => Constant::Null,
            0x02 => {
                let length = next_u32(bc)?;
                let str = String::from_utf8(bc.take(length as usize).cloned().collect())?;
                let actual_length = str.as_bytes().len();
                (actual_length == length as usize).expect(|| anyhow!("expected {} bytes, got {}", length, actual_length))?;
                Constant::String(str)
            },
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
            0x04 => Constant::Slot(next_u16(bc)?),
            0x05 => {
                let n_members = next_u16(bc)?;
                let member_indices = (0..n_members).map(|_| {
                    next_u16(bc)
                }).collect::<Result<_>>()?;

                Constant::Class { member_indices }
            },
            0x06 => Constant::Boolean(bc.next().ok_or_else(eos)? == &0x01),
            _ => return Err(anyhow!("Invalid constant tag: {}", tag)),
        };
        constants.push(constant);
    }

    Ok((constants, code))
}

pub fn serialise(constant_pool: Vec<Constant>, code: Vec<Instr>) -> Result<Vec<u8>> {
    let mut buf = vec![];
    buf.extend((constant_pool.len() as u16).to_le_bytes());
    for constant in constant_pool {
        let (tag, payload): (u8, Vec<u8>) = match constant {
            Constant::Null => (0x01, vec![]),
            Constant::Boolean(b) => (0x06, vec![if b { 0x01 } else { 0x00 }]),
            Constant::Integer(i) => (0x00, i.to_le_bytes().into()),
            Constant::String(s) => {
                let mut payload: Vec<u8> = (s.bytes().len() as u32).to_le_bytes().into();
                payload.extend(s.bytes());
                (0x02, payload)
            },
            Constant::Slot(s) => (0x04, s.to_le_bytes().into()),
            Constant::Class { member_indices } => (0x05, {
                (member_indices.len() as u16).to_le_bytes().into_iter().chain(
                    member_indices.into_iter().flat_map(u16::to_le_bytes)
                ).collect()
            }),
            Constant::Method { name_idx, n_args, n_locals, start, length } => {
                let mut payload: Vec<u8> = name_idx.to_le_bytes().into();
                payload.extend(n_args.to_le_bytes());
                payload.extend(n_locals.to_le_bytes());
                payload.extend((length as u32).to_le_bytes());
                payload.extend(serialise_code(&code[start..start + length])?);
                (0x03, payload)
            },
        };
        buf.push(tag);
        buf.extend(payload);
    }
    Ok(buf)
}

fn instr_to_bcinstr(instr: &Instr) -> BcInstr {
    match instr {
        Instr::Literal(k)         => (Opcode::Literal,      Layout::Constant(*k)),
        Instr::Drop               => (Opcode::Drop,         Layout::Nullary),
        Instr::Print(k, b)        => (Opcode::Print,        Layout::ConstantAndByte(*k, *b)),
        Instr::GetLocal(k)        => (Opcode::GetLocal,     Layout::Constant(*k)),
        Instr::SetLocal(k)        => (Opcode::SetLocal,     Layout::Constant(*k)),
        Instr::GetGlobal(k)       => (Opcode::GetGlobal,    Layout::Constant(*k)),
        Instr::SetGlobal(k)       => (Opcode::SetGlobal,    Layout::Constant(*k)),
        Instr::GetField(k)        => (Opcode::GetField,     Layout::Constant(*k)),
        Instr::SetField(k)        => (Opcode::SetField,     Layout::Constant(*k)),
        Instr::Label(k)           => (Opcode::Label,        Layout::Constant(*k)),
        Instr::Jump(k)            => (Opcode::Jump,         Layout::Constant(*k)),
        Instr::Branch(k)          => (Opcode::Branch,       Layout::Constant(*k)),
        Instr::CallMethod(k, b)   => (Opcode::CallMethod,   Layout::ConstantAndByte(*k, *b)),
        Instr::CallFunction(k, b) => (Opcode::CallFunction, Layout::ConstantAndByte(*k, *b)),
        Instr::Return             => (Opcode::Return,       Layout::Nullary),
        Instr::Array              => (Opcode::Array,        Layout::Nullary),
        Instr::Object(k)          => (Opcode::Object,       Layout::Constant(*k)),
        Instr::LiteralNull          | Instr::LiteralBool(_)     | Instr::LiteralInt(_)
        | Instr::GetGlobalDirect(_) | Instr::SetGlobalDirect(_) | Instr::GetFieldDirect(_)
        | Instr::SetFieldDirect(_)  | Instr::JumpDirect(_)      | Instr::BranchDirect(_)
        => unreachable!("the extended instruction set doesn't have a bytecode representation"),
    }
}

pub fn serialise_code(code: &[Instr]) -> Result<Vec<u8>> {
    let mut buf = vec![];
    for instr in code {
        let (opcode, payload) = instr_to_bcinstr(instr);
        buf.push(opcode.into());
        match payload {
            Layout::Nullary => (),
            Layout::Constant(k) => buf.extend(k.to_le_bytes()),
            Layout::ConstantAndByte(k, b) => {
                buf.extend(k.to_le_bytes());
                buf.push(b);
            },
        }
    }

    Ok(buf)
}

pub fn parse_globals<'a, It: Iterator<Item = &'a u8>>(iter: &mut It) -> Result<Vec<u16>> {
    let len = next_u16(iter)?;
    (0..len).map(|_| next_u16(iter)).collect()
}

pub fn parse_entry_point<'a, It: Iterator<Item = &'a u8>>(iter: &mut It) -> Result<u16> {
    next_u16(iter)
}

pub fn collect_labels(constant_pool: &mut Vec<Constant>, code: Vec<Instr>)
-> Result<(Vec<Instr>, HashMap<String, Pc>)> {
    let mut patched = vec![];
    let mut labels = HashMap::new();

    for (i, instr) in code.into_iter().enumerate() {
        if let Instr::Label(name_idx) = instr {
            if let Ok(name) = constant_pool[name_idx as usize].as_string() {
                let new_pc = patched.len() as Pc;
                // offset all methods that start after this point
                constant_pool.iter_mut().for_each(|c| {
                    if let Constant::Method { start, length, .. } = c {
                        if *start > new_pc { *start -= 1; }
                        else if *start + *length > new_pc { *length -= 1; }
                    }
                });
                labels.insert(name, new_pc);
            } else {
                return Err(anyhow!(
                    "could not find name for label at {} (real PC would be {}) ({} is not a valid index)",
                    i, patched.len(), name_idx
                ));
            }
        } else {
            patched.push(instr);
        }
    }

    Ok((patched, labels))
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
        let layout = match opcode {
            Opcode::Literal      => Layout::constant(bc)?,
            Opcode::Drop         => Layout::nullary(),
            Opcode::Print        => Layout::constant_and_byte(bc)?,
            Opcode::GetLocal     => Layout::constant(bc)?,
            Opcode::SetLocal     => Layout::constant(bc)?,
            Opcode::GetGlobal    => Layout::constant(bc)?,
            Opcode::SetGlobal    => Layout::constant(bc)?,
            Opcode::GetField     => Layout::constant(bc)?,
            Opcode::SetField     => Layout::constant(bc)?,
            Opcode::Label        => Layout::constant(bc)?,
            Opcode::Jump         => Layout::constant(bc)?,
            Opcode::Branch       => Layout::constant(bc)?,
            Opcode::CallMethod   => Layout::constant_and_byte(bc)?,
            Opcode::CallFunction => Layout::constant_and_byte(bc)?,
            Opcode::Return       => Layout::nullary(),
            Opcode::Array        => Layout::nullary(),
            Opcode::Object       => Layout::constant(bc)?,
        };
        Ok(Some((opcode, layout)))
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
            0x00 => Ok(Opcode::Label),
            0x07 => Ok(Opcode::CallMethod),
            0x08 => Ok(Opcode::CallFunction),
            0x09 => Ok(Opcode::SetLocal),
            0x0A => Ok(Opcode::GetLocal),
            0x0B => Ok(Opcode::SetGlobal),
            0x0C => Ok(Opcode::GetGlobal),
            0x0D => Ok(Opcode::Branch),
            0x0E => Ok(Opcode::Jump),
            0x0F => Ok(Opcode::Return),
            0x03 => Ok(Opcode::Array),
            0x04 => Ok(Opcode::Object),
            0x05 => Ok(Opcode::GetField),
            0x06 => Ok(Opcode::SetField),
            _ => Err(anyhow!("invalid opcode: {}", value)),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(opcode: Opcode) -> Self {
        match opcode {
            Opcode::Literal      => 0x01,
            Opcode::Drop         => 0x10,
            Opcode::Print        => 0x02,
            Opcode::Label        => 0x00,
            Opcode::CallMethod   => 0x07,
            Opcode::CallFunction => 0x08,
            Opcode::SetLocal     => 0x09,
            Opcode::GetLocal     => 0x0A,
            Opcode::SetGlobal    => 0x0B,
            Opcode::GetGlobal    => 0x0C,
            Opcode::Branch       => 0x0D,
            Opcode::Jump         => 0x0E,
            Opcode::Return       => 0x0F,
            Opcode::Array        => 0x03,
            Opcode::Object       => 0x04,
            Opcode::GetField     => 0x05,
            Opcode::SetField     => 0x06,
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
            (Opcode::Literal,      Layout::Constant(k)) => Ok(Instr::Literal(k)),
            (Opcode::Drop,         Layout::Nullary) => Ok(Instr::Drop),
            (Opcode::Print,        Layout::ConstantAndByte(k, b)) => Ok(Instr::Print(k, b)),
            (Opcode::GetLocal,     Layout::Constant(k)) => Ok(Instr::GetLocal(k)),
            (Opcode::SetLocal,     Layout::Constant(k)) => Ok(Instr::SetLocal(k)),
            (Opcode::GetGlobal,    Layout::Constant(k)) => Ok(Instr::GetGlobal(k)),
            (Opcode::SetGlobal,    Layout::Constant(k)) => Ok(Instr::SetGlobal(k)),
            (Opcode::GetField,     Layout::Constant(k)) => Ok(Instr::GetField(k)),
            (Opcode::SetField,     Layout::Constant(k)) => Ok(Instr::SetField(k)),
            (Opcode::Label,        Layout::Constant(k)) => Ok(Instr::Label(k)),
            (Opcode::Jump,         Layout::Constant(k)) => Ok(Instr::Jump(k)),
            (Opcode::Branch,       Layout::Constant(k)) => Ok(Instr::Branch(k)),
            (Opcode::CallMethod,   Layout::ConstantAndByte(k, b)) => Ok(Instr::CallMethod(k, b)),
            (Opcode::CallFunction, Layout::ConstantAndByte(k, b)) => Ok(Instr::CallFunction(k, b)),
            (Opcode::Return,       Layout::Nullary) => Ok(Instr::Return),
            (Opcode::Array,        Layout::Nullary) => Ok(Instr::Array),
            (Opcode::Object,       Layout::Constant(k)) => Ok(Instr::Object(k)),
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
