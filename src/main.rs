use std::collections::LinkedList;

mod parser;
use parser::{SExp, sexp};

// fn plus(args: LinkedList<SExp>) -> SExp {

// }

fn main() {
    let ast = parser::sexp(";comment\nnil");
    println!("{:?}", ast)
}
