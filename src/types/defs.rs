use std::str::FromStr;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum BinaryOperator {
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
pub enum Separator {
    Comma,
    Colon,
    Semi,
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

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Comment {
    Comment,
    DocComment,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Arrow {
    RArrow,
    FatArrow,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Dot {
    Dot,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token<'a>(TokenKind, &'a str);

impl<'a> Token<'a> {
    pub fn new(token_kind: TokenKind, s: &'a str) -> Self {
        Self(token_kind, s)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TokenKind {
    Operator(BinaryOperator),
    Delimiter(Delimiter),
    Separator(Separator),
    Keyword(Keyword),
    Name,
    TypeName,
    MacroName,
    Comment(Comment),
    Number(i32),
    String(String),
    Arrow(Arrow),
    Dot(Dot),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseTokenError<'a> {
    InvalidChar(char, &'a str),
    ParseIntError(<i32 as FromStr>::Err, &'a str),
    UnterminatedString,
    InvalidEscape(char),
}
