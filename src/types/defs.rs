use std::str::FromStr;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token<'a>(TokenKind, &'a str);

impl<'a> Token<'a> {
    pub fn new(token_kind: TokenKind, s: &'a str) -> Self {
        Self(token_kind, s)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TokenKind {
    Keyword(Keyword),
    Identifier,
    Literal(Literal),
    Punctuation(Punctuation),
    Delimiter(Delimiter),
    Comment(Comment),
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Keyword {
    If,
    Else,
    Match,
    While,
    Loop,
    True,
    False,
    Let,
    Type,
    Return,
    Gen,
    Func,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Literal {
    Character(char),
    String(String),
    Number(i32),
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Punctuation {
    Add,
    Sub,
    Mul,
    Pow,
    Div,
    Mod,
    Assign,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    Or,
    And,
    Comma,
    Colon,
    Semi,
    RArrow,
    FatArrow,
    Dot,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Delimiter {
    CurlyLeft,
    CurlyRight,
    SquareLeft,
    SquareRight,
    ParLeft,
    ParRight,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Comment {
    Comment,
    DocComment,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseTokenError<'a> {
    InvalidChar(char, &'a str),
    ParseIntError(<i32 as FromStr>::Err, &'a str),
    UnterminatedString,
    InvalidEscape(char),
}
