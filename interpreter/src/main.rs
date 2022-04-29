#![feature(assert_matches)]
#![feature(let_else)]

use std::path::PathBuf;

use clap::Parser;
use interpreter::LoxInterpreter;

pub mod expr_evaluator;
mod interpreter;
pub mod lox_value;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct CliArguments {
    /// Optional script file to execute
    script_file: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = CliArguments::parse();

    let mut interpreter = LoxInterpreter::new();

    if let Some(script_file) = args.script_file {
        interpreter.run_file(&script_file)
    } else {
        interpreter.run_prompt()
    }
}
