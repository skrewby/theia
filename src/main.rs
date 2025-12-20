use std::env;

use crate::{repl::Repl, runner::run_script};

mod ast;
mod builtin;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod runner;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        let filename = &args[1];
        run_script(filename);
    } else {
        let interpreter = Repl::new();
        interpreter.start();
    }
}
