use std::{
    fmt::{self, Display},
    hash::{Hash, Hasher},
    collections::{HashMap, LinkedList},
    str::FromStr
};
use nom::{
    bytes::complete::{tag, take_until, take_while1},
    branch::alt,
    character::complete::{char, one_of, none_of, space0, space1, line_ending},
    number::complete::float,
    combinator::{map, value, eof},
    sequence::{separated_pair, delimited, preceded, terminated},
    multi::{separated_list0, separated_list1, fold_many0},
    error::ErrorKind,
    ErrorConvert,
    IResult,
    Parser,
    Finish
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

// impl FromStr for SExp {
//     type Err = nom::Err<(String, ErrorKind)>;

//     fn from_str(program: &str) -> Result<Self, Self::Err> {
//         //sexp(program).finish().map_err(|err| err.map(|(s,kind)| (String::from(s),kind)))
//         sexp(program).finish().map_err(|err| err.map(String::from))
//     }
// }

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

#[cfg(test)]
mod tests {
    use std::collections::{
        hash_map::RandomState,
        HashMap, LinkedList
    };

    use quickcheck::{Arbitrary, Gen};
    use quickcheck_macros::quickcheck;
    use crate::parser::{SExp, Literal, Number, Quoted, sexp};

    fn gen_range<'a, I>(g: &mut Gen, iter: I) -> I::Item where I: Iterator<Item: Clone> {
        g.choose(Vec::from_iter(iter).as_slice()).unwrap().clone()
    }

    fn arbitary_string_no_whitespace(g: &mut Gen) -> String {
        let size = gen_range(g, 0..100);
        (0..size)
            .map(|_| char::arbitrary(g))
            .filter(|c| !c.is_whitespace())
            .collect()
    }

    impl Arbitrary for Literal {
        fn arbitrary(g: &mut Gen) -> Self {
            match gen_range(g, 0..=5) {
                0 => Literal::Nil,
                1 => Literal::Bool(bool::arbitrary(g)),
                2 => Literal::Char(char::arbitrary(g)),
                3 => Literal::Num(Number(f32::arbitrary(g))),
                4 => Literal::Str(String::arbitrary(g)),
                5 => Literal::Sym(arbitary_string_no_whitespace(g)),
                _ => unreachable!()
            }
        }

        // fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        //     match self {
        //         Literal::Char(c) => Literal::Char(c.shrink()),
        //         Literal::Num(Number(x)) => Literal::Num(Number(x.shrink())),
        //         Literal::Str(s) => Literal::Str(s.shrink()),
        //         Literal::Sym(s) => Literal::Sym(s.shrink()),
        //         x => x
        //     }
        // }
    }

    impl Arbitrary for Quoted {
        fn arbitrary(g: &mut Gen) -> Self {
            if bool::arbitrary(g) {
                Quoted::Sym(arbitary_string_no_whitespace(g))
            } else {
                Quoted::List(LinkedList::arbitrary(g).into_iter().map(SExp::Literal).collect())
            }
        }
    }

    impl Arbitrary for SExp {
        fn arbitrary(g: &mut Gen) -> Self {
            match gen_range(g, 0..=4) {
                0 => SExp::Literal(Literal::arbitrary(g)),
                1 => SExp::Quoted(Quoted::arbitrary(g)),
                2 => SExp::Array(Vec::arbitrary(g).into_iter().map(SExp::Literal).collect()),
                3 => SExp::List(LinkedList::arbitrary(g).into_iter().map(SExp::Literal).collect()),
                4 => { 
                    let dict: HashMap<Literal, Literal> = HashMap::arbitrary(g);
                    SExp::Dict(
                        dict
                            .into_iter()
                            .map(|(k,v)| (k, SExp::Literal(v)))
                            .collect())
                },
                _ => unreachable!()
            }
        }
    }

    #[quickcheck]
    fn show_then_parse_is_identity(ast: SExp) -> bool {
        let ast_str = ast.to_string();
        let parsed = sexp(ast_str.as_str());
        parsed == Ok(("", ast))
    }

    // quickcheck found a failing test case - saved in file parser_test_fail.txt
    // But it's quite complicated test case - customize Arbitary shrink() for SExp to make it simpler
}
