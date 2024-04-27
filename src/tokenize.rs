use lazy_static::lazy_static;
use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
};

#[derive(Debug, Clone)]
pub enum Token {
    Lparen,
    Rparen,
    Lbrack,
    Rbrack,
    Lcurly,
    Rcurly,
    Comma,
    Semi,
    Dot,
    Pound,
    Op(String),
    Symbol(String),
    StrLit(String),
    NumLit(String),
}

lazy_static! {
    static ref SINGLE_CHAR_TOKENS: HashMap<char, Token> = HashMap::from([
        ('(', Token::Lparen),
        (')', Token::Rparen),
        ('[', Token::Lbrack),
        (']', Token::Rbrack),
        ('{', Token::Lcurly),
        ('}', Token::Rcurly),
        (',', Token::Comma),
        (';', Token::Semi),
        ('.', Token::Dot),
        ('#', Token::Pound),
    ]);
    static ref OPERATOR_CHARS: HashSet<char> =
        HashSet::from(['+', '-', '*', '/', '%', '|', '&', '^', '!', '=', '<', '>']);
}

pub struct Tokens<It: Iterator<Item = char>> {
    data: Peekable<It>,
}

impl<It> From<It> for Tokens<It>
where
    It: Iterator<Item = char>,
{
    #[allow(dead_code)]
    fn from(chars: It) -> Self {
        Tokens {
            data: chars.peekable(),
        }
    }
}

fn read_symbol<It>(data: &mut Peekable<It>) -> Token
where
    It: Iterator<Item = char>,
{
    let mut acc = data.next().unwrap().to_string();
    while data.peek().is_some() {
        let c = data.peek().unwrap();
        if c.is_alphanumeric() || *c == '_' {
            acc.push(data.next().unwrap());
        } else {
            break;
        }
    }
    Token::Symbol(acc)
}

fn read_numeric_literal<It>(data: &mut Peekable<It>) -> Token
where
    It: Iterator<Item = char>,
{
    let mut acc = data.next().unwrap().to_string();
    while data.peek().is_some() {
        let c = data.peek().unwrap();
        if c.is_alphanumeric() || *c == '_' || *c == '.' {
            acc.push(data.next().unwrap());
        } else {
            break;
        }
    }
    Token::NumLit(acc)
}

fn read_string_literal<It>(data: &mut Peekable<It>) -> Token
where
    It: Iterator<Item = char>,
{
    let _ = data.next();
    let mut acc = String::new();
    while data.peek().is_some() {
        let c = data.peek().unwrap();
        if *c != '"' {
            acc.push(data.next().unwrap());
        } else {
            let _ = data.next();
            break;
        }
    }
    Token::StrLit(acc)
}

fn read_operator<It>(data: &mut Peekable<It>) -> Token
where
    It: Iterator<Item = char>,
{
    let mut acc = data.next().unwrap().to_string();
    while data.peek().is_some() {
        let c = data.peek().unwrap();
        if OPERATOR_CHARS.contains(c) {
            acc.push(data.next().unwrap());
        } else {
            break;
        }
    }
    Token::Op(acc)
}

impl<It> Iterator for Tokens<It>
where
    It: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while self.data.peek()?.is_whitespace() {
            let _ = self.data.next();
        }
        let next = self.data.peek()?;
        if let Some(token) = SINGLE_CHAR_TOKENS.get(next) {
            let _ = self.data.next();
            Some(token.clone())
        } else if next.is_alphabetic() || *next == '_' {
            Some(read_symbol(&mut self.data))
        } else if next.is_numeric() {
            Some(read_numeric_literal(&mut self.data))
        } else if *next == '"' {
            Some(read_string_literal(&mut self.data))
        } else {
            Some(read_operator(&mut self.data))
        }
    }
}
