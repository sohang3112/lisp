mod parser;

fn main() {
    let ast = parser::sexp("'(+ 1 2 3)");
    println!("{:?}", ast)
}
