use std::collections::HashMap;
use std::collections::HashSet;
use std::iter;

use anyhow::Result;
use smallvec::SmallVec;

use super::Heap;
use super::HeapObject;
use super::Pointer;
use super::Value;
use super::actual_capacity;

pub const GC_THRESHOLD: f32 = 0.75;

type FewPtrs = SmallVec<[Pointer; 16]>;
pub type RenameMap = HashMap<Pointer, Pointer>;

pub fn run_gc(heap: &Heap, roots: Vec<Pointer>) -> Result<(Heap, RenameMap)> {
    gc(heap, roots)
}

fn ptrs<'a, I>(iter: I) -> FewPtrs where I: Iterator<Item = &'a Value> {
    iter.filter_map(|v| match v {
        &Value::Reference(p) => Some(p),
        _ => None,
    }).collect::<FewPtrs>()
}

fn gc(heap: &Heap, mut stack: Vec<Pointer>) -> Result<(Heap, RenameMap)> {
    let mut visited: HashSet<Pointer> = Default::default();
    let mut open: HashSet<Pointer> = stack.iter().copied().collect();
    let mut live = vec![];

    while let Some(ptr) = stack.pop() {
        open.remove(&ptr);
        visited.insert(ptr);
        let obj = heap.read(ptr)?;

        let children = match obj {
            HeapObject::Array(ref vs) => ptrs(vs.iter()),
            HeapObject::Object {
                ref parent,
                ref fields,
                methods: _,
            } => ptrs(
                fields.iter().map(|(_, v)| v)
                .chain(iter::once(parent))
            ),
        };

        children.into_iter()
            .filter(|p| !visited.contains(p))
            .for_each(|p| if !open.contains(&p) {
                open.insert(p);
                stack.push(p);
            });

        live.push((ptr, obj));
    }

    rebuild_heap(heap, live)
}

fn rebuild_heap(heap: &Heap, live: Vec<(Pointer, HeapObject)>) -> Result<(Heap, RenameMap)> {
    heap.trace(format!("rebuilding heap with {} live objects", live.len()));
    let mut heap: Heap = Heap {
        store: Vec::with_capacity(actual_capacity(heap.capacity)),
        log: heap.log.clone(),
        ..*heap
    };

    let mut next_ptr = 0;
    let mut rename_map: RenameMap = Default::default();

    for (ptr, obj) in live.iter() {
        rename_map.insert(*ptr, (next_ptr as u64).into());
        next_ptr += obj.size();
    }

    for (ptr, obj) in live {
        let new_ptr: u64 = rename_map[&ptr].into();
        let ptr: u64 = ptr.into();
        heap.trace(format!("moving {} bytes from {} to {}", obj.size(), ptr, new_ptr));

        let mut replaced = obj.replace_ptrs(&rename_map);
        replaced.modify_ptrs(|rep_ptr| {
            let rep_ptr: u64 = (*rep_ptr).into();
            assert!(rep_ptr < next_ptr as u64);
        });

        heap.set(new_ptr as usize, &replaced)?;
    }

    Ok((heap, rename_map))
}
