use crate::repl::Repl;

mod lexer;
mod repl;
mod token;

fn main() {
    let interpreter = Repl::new();
    interpreter.start();
}
