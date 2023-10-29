use std::fmt::{Display, Formatter};
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
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Pow => write!(f, "**"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "/"),
            Self::Assign => write!(f, "="),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Gt => write!(f, ">"),
            Self::Lt => write!(f, "<"),
            Self::Ge => write!(f, ">="),
            Self::Le => write!(f, "<="),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Delimiter {
    BraceLeft,
    BraceRight,
}

impl Display for Delimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BraceLeft => write!(f, "{{"),
            Self::BraceRight => write!(f, "}}"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Separator {
    Comma,
    Colon,
    Semi,
}

impl Display for Separator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comma => write!(f, ","),
            Self::Colon => write!(f, ":"),
            Self::Semi => write!(f, ";"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Keyword {
    If,
    Else,
    While,
    Loop,
    True,
    False,
    Let,
    Type,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::While => write!(f, "while"),
            Self::Loop => write!(f, "loop"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Let => write!(f, "let"),
            Self::Type => write!(f, "type"),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token<'a>(TokenKind, &'a str);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TokenKind {
    Operator(BinaryOperator),
    Delimiter(Delimiter),
    Separator(Separator),
    Keyword(Keyword),
    Name,
    Type,
    Number(i32),
    String(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseTokenError<'a> {
    InvalidChar(char, &'a str),
    ParseIntError(<i32 as FromStr>::Err, &'a str),
    UnterminatedString,
    InvalidEscape(char),
}

impl<'a> Display for ParseTokenError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidChar(c, _) => write!(f, "Invalid char: {c}"),
            Self::ParseIntError(e, _) => write!(f, "Error parsing int: {e}"),
            Self::UnterminatedString => write!(f, "No string terminator found!"),
            Self::InvalidEscape(c) => write!(f, "Invalid escape \\{c}"),
        }
    }
}

impl<'a> std::error::Error for ParseTokenError<'a> {}

pub fn split_first_token(string: &str) -> Result<Option<(Token, &str)>, ParseTokenError> {
    let trimmed = string.trim();

    if trimmed.is_empty() {
        return Ok(None);
    }

    let mut chars = trimmed.chars();
    match (chars.next(), chars.next()) {
        (None, _) => Ok(None),
        (Some('0'..='9'), _) | (Some('+' | '-'), Some('0'..='9')) => {
            let (token, remainder) = trimmed.split_at(
                trimmed
                    .char_indices()
                    .skip(1)
                    .find(|(_, f)| !(f.is_ascii_digit()))
                    .map(|(i, _)| i)
                    .unwrap_or(trimmed.len()),
            );
            match token.parse() {
                Ok(i) => Ok(Some((Token(TokenKind::Number(i), token), remainder))),
                Err(e) => Err(ParseTokenError::ParseIntError(e, token)),
            }
        }
        (Some('+'), _) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Add),
                &trimmed[..'+'.len_utf8()],
            ),
            &trimmed['+'.len_utf8()..],
        ))),
        (Some('-'), _) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Sub),
                &trimmed[..'-'.len_utf8()],
            ),
            &trimmed['-'.len_utf8()..],
        ))),
        (Some('*'), Some('*')) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Pow),
                &trimmed[..('*'.len_utf8() + '*'.len_utf8())],
            ),
            &trimmed[('*'.len_utf8() + '*'.len_utf8())..],
        ))),
        (Some('*'), _) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Mul),
                &trimmed[..'*'.len_utf8()],
            ),
            &trimmed['*'.len_utf8()..],
        ))),
        (Some('/'), _) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Div),
                &trimmed[..'/'.len_utf8()],
            ),
            &trimmed['/'.len_utf8()..],
        ))),
        (Some('%'), _) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Mod),
                &trimmed[..'%'.len_utf8()],
            ),
            &trimmed['%'.len_utf8()..],
        ))),
        (Some(':'), Some('=')) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Assign),
                &trimmed[..(':'.len_utf8() + '='.len_utf8())],
            ),
            &trimmed[(':'.len_utf8() + '='.len_utf8())..],
        ))),
        (Some('='), Some('=')) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Eq),
                &trimmed[..('='.len_utf8() + '='.len_utf8())],
            ),
            &trimmed[('='.len_utf8() + '='.len_utf8())..],
        ))),
        (Some('!'), Some('=')) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Ne),
                &trimmed[..('!'.len_utf8() + '='.len_utf8())],
            ),
            &trimmed[('!'.len_utf8() + '='.len_utf8())..],
        ))),
        (Some('>'), Some('=')) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Ge),
                &trimmed[..('>'.len_utf8() + '='.len_utf8())],
            ),
            &trimmed[('>'.len_utf8() + '='.len_utf8())..],
        ))),
        (Some('<'), Some('=')) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Le),
                &trimmed[..('<'.len_utf8() + '='.len_utf8())],
            ),
            &trimmed[('<'.len_utf8() + '='.len_utf8())..],
        ))),
        (Some('>'), _) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Gt),
                &trimmed[..('>'.len_utf8())],
            ),
            &trimmed[('>'.len_utf8())..],
        ))),
        (Some('<'), _) => Ok(Some((
            Token(
                TokenKind::Operator(BinaryOperator::Lt),
                &trimmed[..('<'.len_utf8())],
            ),
            &trimmed[('<'.len_utf8())..],
        ))),
        (Some('{'), _) => Ok(Some((
            Token(
                TokenKind::Delimiter(Delimiter::BraceLeft),
                &trimmed[..'{'.len_utf8()],
            ),
            &trimmed['{'.len_utf8()..],
        ))),
        (Some('}'), _) => Ok(Some((
            Token(
                TokenKind::Delimiter(Delimiter::BraceRight),
                &trimmed[..'}'.len_utf8()],
            ),
            &trimmed['}'.len_utf8()..],
        ))),
        (Some(','), _) => Ok(Some((
            Token(
                TokenKind::Separator(Separator::Comma),
                &trimmed[..','.len_utf8()],
            ),
            &trimmed[','.len_utf8()..],
        ))),
        (Some(':'), _) => Ok(Some((
            Token(
                TokenKind::Separator(Separator::Colon),
                &trimmed[..':'.len_utf8()],
            ),
            &trimmed[':'.len_utf8()..],
        ))),
        (Some(';'), _) => Ok(Some((
            Token(
                TokenKind::Separator(Separator::Semi),
                &trimmed[..';'.len_utf8()],
            ),
            &trimmed[';'.len_utf8()..],
        ))),
        (Some('"'), _) => {
            let mut escaped = false;
            let Some(index) = trimmed
                .char_indices()
                .skip(1)
                .find(|(_, c)| match (c, escaped) {
                    ('\\', false) => {
                        escaped = true;
                        false
                    }
                    ('"', false) => true,
                    (_, true) => {
                        escaped = false;
                        false
                    }
                    (_, false) => false,
                })
                .map(|(i, _)| i)
            else {
                return Err(ParseTokenError::UnterminatedString);
            };
            let mut escaped = false;
            match trimmed[1..index]
                .chars()
                .filter_map(|c| match (c, escaped) {
                    ('\\', false) => {
                        escaped = true;
                        None
                    }
                    (cc, true) => {
                        escaped = false;
                        match cc {
                            '\\' => Some(Ok('\\')),
                            'n' => Some(Ok('\n')),
                            't' => Some(Ok('\t')),
                            '0' => Some(Ok('\0')),
                            '"' => Some(Ok('"')),
                            '\'' => Some(Ok('\'')),
                            ccc => Some(Err(ParseTokenError::InvalidEscape(ccc))),
                        }
                    }
                    (c, false) => Some(Ok(c)),
                })
                .collect::<Result<_, _>>()
            {
                Ok(string) => Ok(Some((
                    Token(TokenKind::String(string), &trimmed[..=index]),
                    &trimmed[index + 1..],
                ))),
                Err(e) => Err(e),
            }
        }
        (Some(c), _) if c.is_alphabetic() | (c == '_') => {
            let (token, remainder) = trimmed.split_at(
                trimmed
                    .find(|f: char| !(f.is_alphanumeric() | (f == '_')))
                    .unwrap_or(trimmed.len()),
            );
            Ok(Some((
                match token {
                    "if" => Token(TokenKind::Keyword(Keyword::If), token),
                    "else" => Token(TokenKind::Keyword(Keyword::Else), token),
                    "while" => Token(TokenKind::Keyword(Keyword::While), token),
                    "loop" => Token(TokenKind::Keyword(Keyword::Loop), token),
                    "true" => Token(TokenKind::Keyword(Keyword::True), token),
                    "false" => Token(TokenKind::Keyword(Keyword::False), token),
                    "let" => Token(TokenKind::Keyword(Keyword::Let), token),
                    "type" => Token(TokenKind::Keyword(Keyword::Type), token),
                    ttype if c.is_uppercase() => Token(TokenKind::Type, ttype),
                    name => Token(TokenKind::Name, name),
                },
                remainder,
            )))
        }
        (Some(c), _) => Err(ParseTokenError::InvalidChar(c, &trimmed[..c.len_utf8()])),
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct SplitTokens<'a> {
    remainder: &'a str,
    original: &'a str,
}

impl<'a> SplitTokens<'a> {
    pub fn new(string: &str) -> SplitTokens {
        SplitTokens {
            remainder: string,
            original: string,
        }
    }
}

impl<'a> Iterator for SplitTokens<'a> {
    type Item = Result<Token<'a>, ParseTokenError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        split_first_token(self.remainder)
            .map(|f| {
                f.map(|(token, remainder)| {
                    self.remainder = remainder;
                    token
                })
            })
            .transpose()
    }
}

pub fn split_tokens(string: &str) -> SplitTokens {
    SplitTokens::new(string)
}

fn main() {
    [
        "catfood-45",
        "catfood",
        "67z23",
        "catfood&-45",
        "&",
        " -45 - 45 + +45",
        "if +2 + -2 else x := x - 5 ",
        "if {{10 / {45 + 3}} + {2 * 4}} - +5",
        "日本語a+123",
        "cat- 32432432432432-ref",
        "{2133 ** 21} % 2",
        "let my_string := \"lol\\\"test\";
let xd := 2;",
    ]
    .into_iter()
    .for_each(|string| {
        println!(
            "{string:?}: {:?}",
            split_tokens(string).collect::<Result<Vec<_>, _>>()
        )
    });
}
