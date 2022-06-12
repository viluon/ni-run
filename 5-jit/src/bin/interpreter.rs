#![feature(fn_traits, associated_type_defaults)]

use std::num::NonZeroU64;
use anyhow::Result;
use clap::Parser;

use fml_jit::*;
use interpreter::*;

#[derive(Parser, Debug)]
#[clap()]
struct Args {
    /// Limit the heap to the given number of mebibytes
    #[clap(long)]
    heap_size: Option<NonZeroU64>,

    /// Log heap events to the given path
    #[clap(long)]
    heap_log: Option<String>,

    /// Enable just-in-time compilation
    #[clap(long)]
    jit: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let mut interpreter = Interpreter::load(
        &mut std::io::stdin(),
        args.heap_log,
        args.heap_size.map(|n| (n.get() * 1024 * 1024).try_into().unwrap()),
        args.jit,
    )?;
    if args.jit {
        unsafe {
            jit::INTERPRETER = Some(&mut interpreter);
        }
    }
    interpreter.execute()
}
