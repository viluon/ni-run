#![feature(fn_traits, associated_type_defaults)]

use std::num::NonZeroU64;
use anyhow::Result;
use clap::Parser;

use fml_jit::*;
use interpreter::*;

#[derive(Parser, Debug)]
#[clap()]
struct Args {
    #[clap(long)]
    heap_size: Option<NonZeroU64>,

    #[clap(long)]
    heap_log: Option<String>,

    #[clap(short)]
    jit: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    Interpreter::load(
        &mut std::io::stdin(),
        args.heap_log,
        args.heap_size.map(|n| (n.get() * 1024 * 1024).try_into().unwrap()),
        args.jit,
    )?.execute()
}
