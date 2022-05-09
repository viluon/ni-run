use std::collections::HashMap;
use std::collections::HashSet;
use std::iter;

use anyhow::Result;
use smallvec::SmallVec;

use super::Heap;
use super::HeapObject;
use super::Pointer;
use super::Value;

pub const GC_THRESHOLD: f32 = 0.75;

pub fn run_gc(heap: &Heap, roots: Vec<Pointer>) -> Result<Heap> {
    gc(heap, roots)
}

type FewPtrs = SmallVec<[Pointer; 16]>;

fn ptrs<'a, I>(iter: I) -> FewPtrs where I: Iterator<Item = &'a Value> {
    iter.filter_map(|v| match v {
        &Value::Reference(p) => Some(p),
        _ => None,
    }).collect::<FewPtrs>()
}

fn gc(heap: &Heap, mut stack: Vec<Pointer>) -> Result<Heap> {
    let mut visited: HashSet<Pointer> = Default::default();
    let mut live = vec![];

    while let Some(ptr) = stack.pop() {
        visited.insert(ptr);
        let obj = heap.read(ptr)?;

        {
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
                .for_each(|p| stack.push(p));
        }

        live.push((ptr, obj));
    }

    rebuild_heap(heap, live)
}

fn rebuild_heap(heap: &Heap, live: Vec<(Pointer, HeapObject)>) -> Result<Heap> {
    let mut heap: Heap = Heap { store: Default::default(), ..*heap };
    let mut next_ptr = 0;
    let mut rename_map: HashMap<Pointer, Pointer> = Default::default();

    for (ptr, obj) in live.iter() {
        rename_map.insert(*ptr, (next_ptr as u64).into());
        next_ptr += obj.size();
    }

    for (ptr, obj) in live {
        let new_ptr: u64 = rename_map[&ptr].into();
        heap.set(new_ptr as usize, &obj.replace_ptrs(&rename_map))?;
    }

    Ok(heap)
}
