use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};
use std::collections::{HashMap, LinkedList};
use nom::multi::fold_many0;
use nom::{
    bytes::complete::{tag, take_until, take_while1},
    branch::{alt},
    character::complete::{char, one_of, none_of, space0, space1, line_ending},
    number::complete::float,
    combinator::{map, value, eof},
    sequence::{separated_pair, delimited, preceded, terminated},
    multi::{separated_list0, separated_list1, many1},
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

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ans: String = match self {
            Literal::Nil => "nil".to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::Num(Number(x)) => x.to_string(),
            Literal::Char(c) => format!("{:?}", c),
            Literal::Sym(s) => s.clone(),
            Literal::Str(s) => format!("{:?}", s)
        };
        write!(f, "{}", ans)
    }
}

impl fmt::Display for Quoted {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}", match self {
            Quoted::Sym(s) => s.clone(),
            Quoted::List(lst) => iterable_to_string(lst)
        })
    }
}

impl fmt::Display for SExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ans = match self {
            SExp::Literal(literal) => literal.to_string(),
            SExp::Quoted(x) => x.to_string(),
            SExp::List(lst) => format!("({})", iterable_to_string(lst)),
            SExp::Array(vec) => format!("[{}]", iterable_to_string(vec)),
            SExp::Dict(dict) => dict_to_string(dict)
        };
        write!(f, "{}", ans)
    }
}

pub fn iterable_to_string<'a, I>(iter: I) -> String where I: IntoIterator<Item = &'a SExp> {
    iter
    .into_iter()
    .map(|x| x.to_string())
    .collect::<Vec<String>>()
    .join(" ")
}

fn dict_to_string<K,V>(dict: &HashMap<K,V>) -> String where K: Display, V: Display {
    let ans = dict
                      .into_iter()
                      .map(|(k,v)| format!("{} {}", k, v))
                      .collect::<Vec<String>>()
                      .join(", ");
    format!("{{{}}}", ans)
}

pub fn comment(program: &str) -> IResult<&str, &str> {
    preceded(
        char(';'), 
        terminated(
            take_until("\n"), 
            alt((line_ending, eof))
        )
    )(program)
}

fn comment_or_space(program: &str) -> IResult<&str, &str> {
    alt((
        comment,
        space0
    ))(program)
}

fn symbol(program: &str) -> IResult<&str, String> {
    map(
        take_while1(|c| !" '\"`@~()[]{}".contains(c)), 
        String::from
    )(program)
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

pub fn comma_or_space(program: &str) -> IResult<&str, ()> {
    value(
        (),
        alt((
            delimited(space0, tag(","), space0),
            space1
        ))
    )(program)
}

fn list(program: &str) -> IResult<&str, LinkedList<SExp>> {
    delimited(
        char('('),
        map(
            multi_sexp, 
            |lst| lst.into_iter().collect()
        ),
        char(')')
    )(program)
}

pub fn array(program: &str) -> IResult<&str, Vec<SExp>> {
    delimited(
        char ('['), 
        multi_sexp,
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

pub fn literal(program: &str) -> IResult<&str, Literal> {
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
    preceded(
        comment_or_space,
        alt((
            map(literal, SExp::Literal),
            map(list, SExp::List),
            map(array, SExp::Array),
            map(dict, SExp::Dict),
            map(quoted, SExp::Quoted)
        ))
    )(program)
}

pub fn multi_sexp(program: &str) -> IResult<&str, Vec<SExp>> {
    separated_list0(
        comma_or_space,
         //map(literal, SExp::Literal)
         sexp
    )(program)
}
