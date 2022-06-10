#![feature(fn_traits, associated_type_defaults, unboxed_closures)]
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod ast;
pub mod bc;
pub mod heap;
pub mod jit;
pub mod util;
