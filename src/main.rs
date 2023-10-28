use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Assign,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Assign => write!(f, "="),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Delimiter {
    BraceLeft,
    BraceRight,
    Semicolon,
}

impl Display for Delimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BraceLeft => write!(f, "{{"),
            Self::BraceRight => write!(f, "}}"),
            Self::Semicolon => write!(f, ";"),
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
pub enum Token<'a> {
    Delimiter(Delimiter, &'a str),
    Keyword(Keyword, &'a str),
    Name(&'a str),
    Number(i32, &'a str),
    Operator(BinaryOperator, &'a str),
    String(String, &'a str),
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
                Ok(i) => Ok(Some((Token::Number(i, token), remainder))),
                Err(e) => Err(ParseTokenError::ParseIntError(e, token)),
            }
        }
        (Some('+'), _) => Ok(Some((
            Token::Operator(BinaryOperator::Add, &trimmed[..'+'.len_utf8()]),
            &trimmed['+'.len_utf8()..],
        ))),
        (Some('-'), _) => Ok(Some((
            Token::Operator(BinaryOperator::Sub, &trimmed[..'-'.len_utf8()]),
            &trimmed['-'.len_utf8()..],
        ))),
        (Some('*'), _) => Ok(Some((
            Token::Operator(BinaryOperator::Mul, &trimmed[..'*'.len_utf8()]),
            &trimmed['*'.len_utf8()..],
        ))),
        (Some('/'), _) => Ok(Some((
            Token::Operator(BinaryOperator::Div, &trimmed[..'/'.len_utf8()]),
            &trimmed['/'.len_utf8()..],
        ))),
        (Some(':'), Some('=')) => Ok(Some((
            Token::Operator(
                BinaryOperator::Assign,
                &trimmed[..(':'.len_utf8() + '='.len_utf8())],
            ),
            &trimmed[(':'.len_utf8() + '='.len_utf8())..],
        ))),
        (Some('{'), _) => Ok(Some((
            Token::Delimiter(Delimiter::BraceLeft, &trimmed[..'{'.len_utf8()]),
            &trimmed['{'.len_utf8()..],
        ))),
        (Some('}'), _) => Ok(Some((
            Token::Delimiter(Delimiter::BraceRight, &trimmed[..'}'.len_utf8()]),
            &trimmed['}'.len_utf8()..],
        ))),
        (Some(';'), _) => Ok(Some((
            Token::Delimiter(Delimiter::Semicolon, &trimmed[..';'.len_utf8()]),
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
                            '"' => Some(Ok('"')),
                            ccc => Some(Err(ParseTokenError::InvalidEscape(ccc))),
                        }
                    }
                    (c, false) => Some(Ok(c)),
                })
                .collect::<Result<_, _>>()
            {
                Ok(string) => Ok(Some((
                    Token::String(string, &trimmed[..=index]),
                    &trimmed[index + 1..],
                ))),
                Err(e) => Err(e),
            }
        }
        (Some(c), _) if c.is_alphanumeric() | (c == '_') => {
            let (token, remainder) = trimmed.split_at(
                trimmed
                    .find(|f: char| !(f.is_alphanumeric() | (f == '_')))
                    .unwrap_or(trimmed.len()),
            );
            Ok(Some((
                match token {
                    "if" => Token::Keyword(Keyword::If, token),
                    "else" => Token::Keyword(Keyword::Else, token),
                    "while" => Token::Keyword(Keyword::While, token),
                    "loop" => Token::Keyword(Keyword::Loop, token),
                    "true" => Token::Keyword(Keyword::True, token),
                    "false" => Token::Keyword(Keyword::False, token),
                    "let" => Token::Keyword(Keyword::Let, token),
                    "type" => Token::Keyword(Keyword::Type, token),
                    name => Token::Name(name),
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
