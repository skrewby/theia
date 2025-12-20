use crate::repl::Repl;

mod ast;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    let interpreter = Repl::new();
    interpreter.start();
}
