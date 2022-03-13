#![feature(fn_traits, associated_type_defaults)]
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod ast;
pub mod bc;
pub mod state;
