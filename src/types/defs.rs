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
    Ident,
    ProperIdent,
    Literal(Literal),
    Punct(Punct),
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
    Ret,
    Gen,
    Func,
    Where,
    Miguel,
    Kyasig,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Literal {
    Character(char),
    String(String),
    Number,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Punct {
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    StarStar,
    Percent,
    Caret,
    Not,
    And,
    Or,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    At,
    Dot,
    Comma,
    Semi,
    Colon,
    ColonColon,
    RArrow,
    FatArrow,
    ForAll,
    Exists,
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
    CapsInImproperIdent(&'a str, usize),
    UnderscoreInProper(&'a str, usize),
    UnterminatedString,
    InvalidEscape(char),
}
