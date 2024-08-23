use std::hash::{Hash, Hasher};
use std::collections::{HashMap, LinkedList};
use nom::multi::fold_many0;
use nom::{
    bytes::complete::{tag, take_while_m_n},
    branch::{alt},
    character::complete::{char, one_of, none_of, space0, space1},
    number::complete::float,
    combinator::{map, value},
    sequence::{separated_pair, delimited, preceded},
    multi::separated_list1,
    IResult,
    Parser,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Number(f32);

impl Eq for Number {}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Literal {
    Nil,
    Bool(bool),
    Num(Number),
    Char(char),
    Sym(String),
    Str(String)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Quoted {
    Sym(String),
    List(LinkedList<SExp>)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SExp {
    Literal(Literal),
    List(LinkedList<SExp>),
    Array(Vec<SExp>),
    Dict(HashMap<Literal, SExp>),
    Quoted(Quoted)
}

fn symbol(program: &str) -> IResult<&str, String> {
    map(none_of(" '\"`@~()[]{}"), String::from)(program)
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

fn comma_or_space(program: &str) -> IResult<&str, &str> {
    alt((
        preceded(char(','), space0),
        space1
    ))(program)
}

fn list(program: &str) -> IResult<&str, LinkedList<SExp>> {
    delimited(
        char('('),
        map(
            separated_list1(comma_or_space, sexp), 
            |lst| lst.into_iter().collect()
        ),
        char(')')
    )(program)
}

fn array(program: &str) -> IResult<&str, Vec<SExp>> {
    delimited(
        char ('['), 
        separated_list1(comma_or_space, sexp),
        char(']')
    )(program)
}

fn dict(program: &str) -> IResult<&str, HashMap<Literal, SExp>> {
    delimited(
        char('{'),
        map(
            separated_list1(
                comma_or_space,
                separated_pair(
                    literal,
                    space1,
                    sexp
                )
            ),
            |tuples| tuples.into_iter().collect()
        ),
        char('}')
    )(program)
}

fn literal(program: &str) -> IResult<&str, Literal> {
    alt((
        value(Literal::Nil, tag("nil")),
        value(Literal::Bool(true), tag("true")),
        value(Literal::Bool(false), tag("false")),
        map(float, |x| Literal::Num(Number(x))),  // TODO: allow binary literal (eg. 0b1011), hex literal (eg. 0xFFF)
        map(symbol, Literal::Sym),
        map(delimited(char('\''), character, char('\'')), Literal::Char),
        map(string_literal, Literal::Str)
    ))(program)
}

fn quoted(program: &str) -> IResult<&str, Quoted> {
    preceded(
        char('\''),
        alt((
            map(symbol, Quoted::Sym),
            map(list, Quoted::List)
        ))
    )(program)
}

pub fn sexp(program: &str) -> IResult<&str, SExp> {
    alt((
        map(literal, SExp::Literal),
        map(list, SExp::List),
        map(array, SExp::Array),
        map(dict, SExp::Dict),
        map(quoted, SExp::Quoted)
    ))(program)
}
