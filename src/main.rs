use std::collections::LinkedList;

mod parser;
use parser::{SExp, sexp};

// fn plus(args: LinkedList<SExp>) -> SExp {

// }

fn main() {
    let ast = sexp("{'a' 1 'b' 2}");
    println!("{:?}", ast)
}
