use std::fmt::{Display, Formatter};

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
pub enum Keyword {
    If,
    Else,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token<'a> {
    Keyword(Keyword),
    Name(&'a str),
    Number(i32),
    Operator(BinaryOperator),
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct ParseTokenError {}

pub fn split_first_token(string: &str) -> Result<(Token, &str), ParseTokenError> {
    match string.chars().next() {
        None => Err(ParseTokenError {}),
        Some('+') => Ok((Token::Operator(BinaryOperator::Add), string.split_at(1).1)),
        Some('-') => Ok((Token::Operator(BinaryOperator::Sub), string.split_at(1).1)),
        Some('*') => Ok((Token::Operator(BinaryOperator::Mul), string.split_at(1).1)),
        Some('/') => Ok((Token::Operator(BinaryOperator::Div), string.split_at(1).1)),
        Some('=') => Ok((
            Token::Operator(BinaryOperator::Assign),
            string.split_at(1).1,
        )),
        Some('0'..='9') => {
            let (token, remainder) = string.split_at(
                string
                    .find(|f: char| !(f.is_ascii_digit()))
                    .unwrap_or(string.len()),
            );
            Ok((
                Token::Number(token.parse::<i32>().map_err(|e| ParseTokenError {})?),
                remainder,
            ))
        }
        Some(c) => {
            if !(c.is_alphanumeric() | (c == '_')) {
                return Err(ParseTokenError {});
            }
            let (token, remainder) = string.split_at(
                string
                    .find(|f: char| !(f.is_alphanumeric() | (f == '_')))
                    .unwrap_or(string.len()),
            );
            Ok((
                match token {
                    "if" => Token::Keyword(Keyword::If),
                    "else" => Token::Keyword(Keyword::Else),
                    word => Token::Name(word),
                },
                remainder,
            ))
        }
    }
}

pub struct SplitTokens<'a> {
    remainder: &'a str,
}

impl<'a> SplitTokens<'a> {
    pub fn new(string: &str) -> SplitTokens {
        SplitTokens { remainder: string }
    }
}

impl<'a> Iterator for SplitTokens<'a> {
    type Item = Result<Token<'a>, ParseTokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remainder.is_empty() {
            return None;
        }
        Some(split_first_token(self.remainder).map(|(token, remainder)| {
            self.remainder = remainder;
            token
        }))
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
        "&catfood-45",
        "catfood&-45",
        "catfood-45&",
        "&",
        " catfood -45 67z23",
        "if catfood-45 = 6723 else xd 324if3432",
    ]
    .into_iter()
    .for_each(|string| {
        println!(
            "{string}: {:?}",
            split_tokens(string).collect::<Result<Vec<_>, _>>()
        )
    });
}
