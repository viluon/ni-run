use std::collections::HashMap;
use itertools::Itertools;
use anyhow::Result;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pointer(u16, u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(i32),
    Bool(bool),
    Reference(Pointer),
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HeapObject {
    Array(Vec<Value>), // TODO: copy on write?
    Object { parent: Pointer, fields: Vec<(u16, Value)>, methods: HashMap<u16, u16> },
}

pub enum HeapTag {
    Array, Object,
}

impl From<&Value> for [u8; 8] {
    fn from(value: &Value) -> [u8; 8] {
        let (tag, repr) = match value {
            Value::Int(i) => (1, match i.to_le_bytes() { [a1, a2, a3, a4] => [a1, a2, a3, a4, 0, 0] }),
            Value::Bool(b) => (2, [if *b { 1 } else { 0 }, 0, 0, 0, 0, 0]),
            Value::Null => (4, [0; 6]),
            Value::Reference(addr) => (5, addr.to_le_bytes()),
        };
        [tag, repr[0], repr[1], repr[2], repr[3], repr[4], repr[5], 0]
    }
}

impl Value {
    fn to_le_bytes(&self) -> [u8; 8] {
        self.into()
    }

    fn from_le_bytes(bytes: &[u8]) -> Value {
        Value::from(bytes)
    }
}

impl Pointer {
    #[inline(always)]
    fn u64(&self) -> u64 {
        (self.0 as u64) << 32 | self.1 as u64
    }

    fn from_le_bytes(bytes: &[u8; 6]) -> Pointer {
        Pointer(u16::from_le_bytes(bytes[0..2].try_into().unwrap()), u32::from_le_bytes(bytes[2..6].try_into().unwrap()))
    }

    fn to_le_bytes(&self) -> [u8; 6] {
        let [a1, a2] = self.0.to_le_bytes();
        let [a3, a4, a5, a6] = self.1.to_le_bytes();
        [a1, a2, a3, a4, a5, a6]
    }
}

impl From<&HeapObject> for Vec<u8> {
    fn from(obj: &HeapObject) -> Self {
        let (tag, mut repr) = match obj {
            HeapObject::Array(v) => (3, vec![
                (v.len() as u64).to_le_bytes().to_vec(),
                v.iter().flat_map(|x| x.to_le_bytes().into_iter()).collect()
            ].concat()),
            HeapObject::Object { parent, fields, methods } => (6, {vec![
                (parent.to_le_bytes().to_vec()),
                vec![0, 0], // padding
                (fields.len() as u64).to_le_bytes().to_vec(),
                (methods.len() as u64).to_le_bytes().to_vec(),
                fields.iter().flat_map(|(k, v)| {
                    k.to_le_bytes().iter().copied()
                    .chain(v.to_le_bytes().iter().copied())
                    .collect_vec()
                }).collect_vec(),
                methods.iter().flat_map(|(k, v)| {
                    k.to_le_bytes().iter().copied()
                    .chain(v.to_le_bytes().iter().copied())
                    .collect_vec()
                }).collect_vec(),
            ].concat()}),
        };
        repr.insert(0, tag);
        repr
    }
}

impl From<&[u8]> for Value {
    fn from(bytes: &[u8]) -> Self {
        let tag = bytes[0];
        let bytes = &bytes[1..];
        match tag {
            1 => Value::Int(i32::from_le_bytes(bytes[..4].try_into().unwrap())),
            2 => Value::Bool(bytes[0] == 1),
            4 => Value::Null,
            5 => Value::Reference(Pointer::from_le_bytes(bytes[..6].try_into().unwrap())),
            _ => panic!("Invalid value tag"),
        }
    }
}

impl From<&[u8]> for HeapObject {
    fn from(bytes: &[u8]) -> Self {
        let tag = bytes[0];
        let bytes = &bytes[1..];
        match tag {
            3 => HeapObject::Array({
                let len = u64::from_le_bytes(bytes[..8].try_into().unwrap());
                let mut v = Vec::with_capacity(len as usize);
                for i in 1..=len as usize {
                    v.push(Value::from_le_bytes(bytes[(i * 8)..((i + 1) * 8)].try_into().unwrap()));
                }
                v
            }),
            6 => {
                let parent = Pointer::from_le_bytes(bytes[..6].try_into().unwrap());
                let len_fields = u64::from_le_bytes(bytes[8..16].try_into().unwrap()) as usize;
                let len_methods = u64::from_le_bytes(bytes[16..24].try_into().unwrap()) as usize;
                let mut bytes = &bytes[24..];
                let mut fields = Vec::with_capacity(len_fields);
                let mut methods = HashMap::with_capacity(len_methods);
                for _ in 0..len_fields {
                    let key = u16::from_le_bytes(bytes[..2].try_into().unwrap());
                    let value = Value::from_le_bytes(bytes[2..10].try_into().unwrap());
                    fields.push((key, value));
                    bytes = &bytes[10..];
                }
                for _ in 0..len_methods {
                    let key = u16::from_le_bytes(bytes[..2].try_into().unwrap());
                    let value = u16::from_le_bytes(bytes[2..4].try_into().unwrap());
                    methods.insert(key, value);
                    bytes = &bytes[4..];
                }
                HeapObject::Object { parent, fields, methods }
            },
            _ => panic!("Invalid heap object tag"),
        }
    }
}

impl Value {
    pub fn as_bool<F: FnOnce(&Value) -> anyhow::Error>(&self, err: F) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            v => Err(err(v)),
        }
    }

    pub fn as_int<F: FnOnce(&Value) -> anyhow::Error>(&self, err: F) -> Result<i32> {
        match self {
            Value::Int(i) => Ok(*i),
            v => Err(err(v)),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Null)
    }
}
