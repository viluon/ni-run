pub mod gc;

use std::collections::HashMap;
use std::num::NonZeroU64;

use itertools::Itertools;
use anyhow::Result;
use anyhow::anyhow;
use smallvec::SmallVec;

use crate::util::BooleanAssertions;

#[derive(Debug, Default, Clone)]
pub struct Heap {
    capacity: Option<NonZeroU64>,
    store: Vec<u8>,
}

impl Heap {
    pub fn should_gc_before_alloc(&self, obj: &HeapObject) -> bool {
        self.capacity.map(|cap|
            (self.store.len() + obj.size()) as f32 > (gc::GC_THRESHOLD * cap.get() as f32)
        ).unwrap_or(false)
    }

    pub fn gc(&mut self, roots: Vec<Pointer>) -> Result<()> {
        let new = gc::run_gc(self, roots)?;
        self.store = new.store;
        self.capacity = new.capacity;
        Ok(())
    }

    /// Unfortunately, this function doesn't trigger garbage collection,
    /// since the GC roots come from the interpreter.
    pub fn alloc_after_gc(&mut self, obj: &HeapObject) -> Result<Pointer> {
        let addr = self.store.len();
        self.set(addr, obj)?;
        Ok(Pointer::from(addr as u64))
    }

    pub fn read(&self, addr: Pointer) -> Result<HeapObject> {
        let addr: u64 = addr.into();
        let addr = addr as usize;
        if addr < self.store.len() {
            Ok(HeapObject::from(&self.store[addr..]))
        } else { Err(anyhow!("invalid address: {:#06x}", addr)) }
    }

    pub fn read_tag(&self, addr: Pointer) -> Result<HeapTag> {
        let addr: u64 = addr.into();
        let addr = addr as usize;
        if addr < self.store.len() {
            Ok(HeapTag::from(&self.store[addr..]))
        } else { Err(anyhow!("invalid address: {:#06x}", addr)) }
    }

    pub fn array_set(&mut self, arr: Pointer, i: u32, v: Value) -> Result<()> {
        let heap = self.store.as_mut_slice();
        let arr: u64 = arr.into();
        let offset = arr as usize + 1 + 8 + i as usize * 8;
        (offset + 8 <= heap.len()).expect(|| anyhow!("array index out of bounds"))?;
        heap[offset .. offset + 8].copy_from_slice(&v.to_le_bytes());
        Ok(())
    }

    pub fn array_get(&self, arr: Pointer, i: u32) -> Result<Value> {
        let heap = self.store.as_slice();
        let arr: u64 = arr.into();
        let offset = arr as usize + 1 + 8 + i as usize * 8;
        (offset + 8 <= heap.len()).expect(|| anyhow!("array index out of bounds"))?;
        Ok(Value::from_le_bytes(heap[offset .. offset + 8].try_into().unwrap()))
    }

    pub fn get_field(&mut self, ptr: Pointer, name: u16, str_name: &str) -> Result<&mut [u8/*; 8*/]> {
        use HeapObject::*;
        // FIXME avoid cloning
        match self.read(ptr)? {
            Object { parent: _, fields, methods: _ } => {
                let index = fields.binary_search_by_key(&name, |p| p.0)
                    // TODO tail-recurse for parent
                    .map_err(|_| anyhow!("no such field {}", str_name))?;

                let addr: u64 = ptr.into();
                let field_value_addr = addr as usize
                    + 1  // tag
                    + 24 // parent, field and method counts
                    + index as usize * 10 // field index
                    + 2; // skip key
                Ok(self.store[field_value_addr .. field_value_addr + 8].as_mut())
            },
            _ => Err(anyhow!("not an object")),
        }
    }

    fn set(&mut self, addr: usize, obj: &HeapObject) -> Result<()> {
        let repr: Vec<u8> = obj.into();
        if self.store.len() < addr + repr.len() {
            if self.capacity.map(|n| (n.get() as usize) < addr + repr.len()).unwrap_or(true) {
                self.store.resize(addr + repr.len(), 0);
            } else {
                return Err(anyhow!("out of memory"));
            }
        }
        self.store[addr..addr + repr.len()].copy_from_slice(&repr);
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pointer(u16, u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(i32),
    Bool(bool),
    Reference(Pointer),
    Null,
}

pub type ObjectFields = SmallVec<[(u16, Value); 4]>;
pub type ObjectMethods = SmallVec<[(u16, u16); 6]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HeapObject {
    Array(SmallVec<[Value; 9]>),
    Object { parent: Value, fields: ObjectFields, methods: ObjectMethods },
}

pub enum HeapTag {
    Array(u64), Object,
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
    pub fn to_le_bytes(&self) -> [u8; 8] {
        self.into()
    }

    fn from_le_bytes(bytes: &[u8]) -> Value {
        Value::from(bytes)
    }
}

impl Pointer {
    fn from_le_bytes(bytes: &[u8; 6]) -> Pointer {
        Pointer(u16::from_le_bytes(bytes[0..2].try_into().unwrap()), u32::from_le_bytes(bytes[2..6].try_into().unwrap()))
    }

    fn to_le_bytes(self) -> [u8; 6] {
        let [a1, a2] = self.0.to_le_bytes();
        let [a3, a4, a5, a6] = self.1.to_le_bytes();
        [a1, a2, a3, a4, a5, a6]
    }
}

impl From<u64> for Pointer {
    fn from(addr: u64) -> Pointer {
        Pointer((addr >> 32) as u16, addr as u32)
    }
}

impl From<Pointer> for u64 {
    fn from(ptr: Pointer) -> u64 {
        (ptr.0 as u64) << 32 | (ptr.1 as u64)
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

impl From<&[u8]> for HeapTag {
    fn from(bytes: &[u8]) -> Self {
        let tag = bytes[0];
        match tag {
            3 => HeapTag::Array(u64::from_le_bytes(bytes[1..9].try_into().unwrap())),
            6 => HeapTag::Object,
            _ => panic!("Invalid heap tag"),
        }
    }
}

impl HeapObject {
    pub fn replace_ptrs(self, rename_map: &HashMap<Pointer, Pointer>) -> Self {
        let patch_value = |v| match v {
            Value::Reference(p) => Value::Reference(rename_map[&p]),
            _ => v,
        };
        match self {
            HeapObject::Array(v) =>
                HeapObject::Array(v.into_iter().map(|x| patch_value(x)).collect()),
            HeapObject::Object { parent, fields, methods } =>
                HeapObject::Object {
                    parent,
                    fields: fields.into_iter().map(|(k, v)| (k, patch_value(v))).collect(),
                    methods
                },
        }
    }

    // The size of this object on the heap.
    pub fn size(&self) -> usize {
        use HeapObject::*;
        1 + match self {
            Array(arr) =>
                8 + arr.len() * 8,
            Object { parent: _, fields, methods } =>
                3 * 8 + fields.len() * (2 + 8) + methods.len() * (2 + 2),
        }
    }
}

// WARN: keep in sync with the above
impl From<&[u8]> for HeapObject {
    fn from(bytes: &[u8]) -> Self {
        let tag = bytes[0];
        let bytes = &bytes[1..];
        match tag {
            3 => HeapObject::Array({
                let len = u64::from_le_bytes(bytes[..8].try_into().unwrap());
                let mut v = SmallVec::with_capacity(len as usize);
                for i in 1..=len as usize {
                    v.push(Value::from_le_bytes(bytes[(i * 8)..((i + 1) * 8)].try_into().unwrap()));
                }
                v
            }),
            6 => {
                let parent = Value::from_le_bytes(bytes[..8].try_into().unwrap());
                let len_fields = u64::from_le_bytes(bytes[8..16].try_into().unwrap()) as usize;
                let len_methods = u64::from_le_bytes(bytes[16..24].try_into().unwrap()) as usize;
                let mut bytes = &bytes[24..];
                let mut fields = SmallVec::with_capacity(len_fields);
                let mut methods = SmallVec::with_capacity(len_methods);
                for _ in 0..len_fields {
                    let key = u16::from_le_bytes(bytes[..2].try_into().unwrap());
                    let value = Value::from_le_bytes(bytes[2..10].try_into().unwrap());
                    fields.push((key, value));
                    bytes = &bytes[10..];
                }
                for _ in 0..len_methods {
                    let key = u16::from_le_bytes(bytes[..2].try_into().unwrap());
                    let value = u16::from_le_bytes(bytes[2..4].try_into().unwrap());
                    methods.push((key, value));
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

    pub fn as_reference<F: FnOnce(&Value) -> anyhow::Error>(&self, err: F) -> Result<Pointer> {
        match self {
            Value::Reference(p) => Ok(*p),
            v => Err(err(v)),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Null)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_type_sizes() {
        use std::mem::size_of;
        assert_eq!(size_of::<Pointer>(), 8);
        assert_eq!(size_of::<Value>(), 12);
        assert_eq!(size_of::<ObjectFields>(), 80);
        assert_eq!(size_of::<ObjectMethods>(), 40);
        assert_eq!(size_of::<Vec<Value>>(), 24);
        assert_eq!(size_of::<HeapObject>(), 136);
        assert_eq!(size_of::<HeapTag>(), 16);
    }
}
