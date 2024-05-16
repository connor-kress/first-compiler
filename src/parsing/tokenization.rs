use lazy_static::lazy_static;
use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimativeType {
    Int,
    Float,
    Bool,
    Char,
    Void,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    // Pound,
    For,
    If,
    Else,
    While,
    Return,
    // Include,
    Op(String),
    Symbol(String),
    StrLit(String),
    NumLit(String),

    Type(PrimativeType),
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
        // ('#', Token::Pound),
    ]);
    static ref OPERATOR_CHARS: HashSet<char> =
        HashSet::from(['+', '-', '*', '/', '%', '|', '&', '^', '~', '!', '=', '<', '>']);
    static ref KEYWORDS: HashMap<&'static str, Token> = HashMap::from([
        ("for", Token::For),
        ("if", Token::If),
        ("else", Token::Else),
        ("while", Token::While),
        ("return", Token::Return),
        // ("include", Token::Include),
    ]);
    static ref PRIMATIVE_TYPES: HashMap<&'static str, PrimativeType> = HashMap::from([
        ("int", PrimativeType::Int),
        ("float", PrimativeType::Float),
        ("bool", PrimativeType::Bool),
        ("char", PrimativeType::Char),
        ("void", PrimativeType::Void),
    ]);
}

pub struct Tokens<It: Iterator<Item = char>> {
    data: Peekable<It>,
}

impl<It: Iterator<Item = char>> From<It> for Tokens<It> {
    #[allow(dead_code)]
    fn from(chars: It) -> Self {
        Tokens {
            data: chars.peekable(),
        }
    }
}

fn read_alphabetic<It: Iterator<Item = char>>(data: &mut Peekable<It>) -> Token {
    let mut acc = data.next().unwrap().to_string();
    while data.peek().is_some() {
        let c = data.peek().unwrap();
        if c.is_alphanumeric() || *c == '_' {
            acc.push(data.next().unwrap());
        } else {
            break;
        }
    }
    if let Some(token) = KEYWORDS.get(acc.as_str()) {
        token.clone()
    } else if let Some(prim) = PRIMATIVE_TYPES.get(acc.as_str()) {
        Token::Type(prim.clone())
    } else {
        Token::Symbol(acc)
    }
}

fn read_numeric_literal<It: Iterator<Item = char>>(data: &mut Peekable<It>) -> Token {
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

fn read_string_literal<It: Iterator<Item = char>>(data: &mut Peekable<It>) -> Token {
    let _ = data.next().unwrap();
    let mut acc = String::new();
    while data.peek().is_some() {
        let c = data.peek().unwrap();
        if *c != '"' {
            acc.push(data.next().unwrap());
        } else {
            let _ = data.next().unwrap();
            break;
        }
    }
    Token::StrLit(acc)
}

fn read_operator<It: Iterator<Item = char>>(data: &mut Peekable<It>) -> Token {
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

impl<It: Iterator<Item = char>> Iterator for Tokens<It> {
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
            Some(read_alphabetic(&mut self.data))
        } else if next.is_numeric() {
            Some(read_numeric_literal(&mut self.data))
        } else if *next == '"' {
            Some(read_string_literal(&mut self.data))
        } else {
            Some(read_operator(&mut self.data))
        }
    }
}
