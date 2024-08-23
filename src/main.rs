use core::fmt;
use std::{
    collections::{LinkedList, HashMap}, 
    vec
};

mod parser;
use parser::{SExp, Literal, sexp};

enum Value {
    Val(SExp),
    Builtin(Box<dyn Fn(&VarMap, Vec<SExp>) -> SExp>),
    UserFunc(Vec<SExp>, Vec<SExp>)
}
type VarMap = HashMap<String, Value>;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ans = match self {
            Value::Val(sexp) => sexp.to_string(),
            Value::Builtin(_) => String::from("<BUILTIN_FUNCTION>"),
            Value::UserFunc(func_params, func_body) => 
                format!("(fn [{}] {})", 
                        parser::iterable_to_string(func_params), 
                        parser::iterable_to_string(func_body))
        };
        write!(f, "{}", ans)
    }
}

fn print_sexp(globals: &VarMap, args: Vec<SExp>) -> SExp {
    for x in args {
        print!("{}", x)
    }
    SExp::Literal(Literal::Nil)
}

fn variable(globals: &VarMap, variable_name: String) -> &Value {
    globals.get(&variable_name).expect(&format!("Variable not defined: {:?}", variable_name))
}

fn apply(globals: &VarMap, function_name: String, args: Vec<SExp>) -> SExp {
    match variable(globals, function_name) {
        Value::Val(_) => panic!("Not a function"),
        Value::Builtin(func) => func(globals, args),
        Value::UserFunc(func_params, func_body) => todo!()
    }
}

fn eval(globals: &VarMap, sexp: SExp) -> Value {
    match sexp {
        SExp::List(lst) => match lst.pop_front() {
            None => panic!("Empty function call"),
            Some(func) => match eval(globals, func) {
                SExp::Literal(Literal::Sym(sym)) => ,
                x => 
            }
        },
        SExp::Quoted(x) => x,
        SExp::Literal(Literal::Sym(var)) => variable(globals, var),
        _  => sexp
    }
}

fn main() {
    let mut globals: VarMap = HashMap::from([
        (String::from("print"), Value::Builtin(Box::new(print_sexp)))
    ]);
    let (rem, ast) = parser::sexp("{a 0, main (fn [x y] '(0 \"Hello World\\n\"))}").unwrap();
    println!("{}\n{}", ast, rem)
    // println!("{:?}", parser::comma_or_space(","));
    // println!("{:?}", parser::comma_or_space(", "));
    // println!("{:?}", parser::comma_or_space(" ,"));
    // println!("{:?}", parser::comma_or_space(" , "));
    // println!("{:?}", parser::comma_or_space("  "));
}
