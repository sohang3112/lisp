use std::fmt::Display;
use std::collections::{HashMap, LinkedList};
use nom::multi::fold_many0;
use nom::{
    bytes::complete::{tag, take_while_m_n},
    branch::{alt},
    character::complete::{alpha1, char, one_of, none_of, space1},
    number::complete::float,
    combinator::{map, value},
    sequence::{Tuple, delimited, preceded},
    multi::separated_list1,
    IResult,
    Parser,
};

#[derive(Debug, PartialEq, Clone)]
enum SExp {
    Nil,
    Bool(bool),
    Num(f32),
    Char(char),
    Sym(String),
    Str(String),
    List(LinkedList<SExp>)
}

fn character(program: &str) -> IResult<&str, char> {
    alt((
        preceded(
            char('\\'),
            alt((
                map(one_of(r#"nrt"\"#), |c| match c {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\'' => '\'',
                    '"' => '"',
                    '\\' => '\\',
                    _ => unreachable!(),
                }),
            )),
        ),
        none_of("\\\"")
    ))(program)
}

fn string_literal(program: &str) -> IResult<&str, String> {
    delimited(
        char('"'),
        fold_many0(
            character,
            String::new,
            |mut acc, ch| { acc.push(ch); acc }
        ),
        char('"')
    )(program)
}

fn list(program: &str) -> IResult<&str, LinkedList<SExp>> {
    delimited(
        char('('),
        map(
            separated_list1(space1, sexp), 
            |lst| lst.into_iter().collect()
        ),
        char(')')
    )(program)
}

fn sexp(program: &str) -> IResult<&str, SExp> {
    alt((
        value(SExp::Nil, tag("nil")),
        value(SExp::Bool(true), tag("true")),
        value(SExp::Bool(false), tag("false")),
        map(float, SExp::Num),  // TODO: allow binary literal (eg. 0b1011), hex literal (eg. 0xFFF)
        map(none_of(" '\"`@~()[]{}"), |s| SExp::Sym(String::from(s))),
        map(delimited(char('\''), character, char('\'')), SExp::Char),
        map(string_literal, SExp::Str),
        map(list, SExp::List)
    ))(program)
}

fn main() {
    let ast = sexp("(+ 1 2)");
    println!("{:?}", ast)
}
