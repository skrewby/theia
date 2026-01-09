use std::env;

use crate::{repl::Repl, runner::run_script};

mod ast;
mod builtin;
mod compiler;
mod lexer;
mod object;
mod opcode;
mod parser;
mod repl;
mod runner;
mod token;
mod vm;

fn repl_mode() {
    let interpreter = Repl::new();
    interpreter.start();
}

fn compile_and_run(args: &Vec<String>) {
    let filename = &args[1];
    if let Err(errors) = run_script(filename) {
        for e in errors {
            println!("{}", e);
        }
        std::process::exit(1);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        2 => compile_and_run(&args),
        _ => repl_mode(),
    }
}
