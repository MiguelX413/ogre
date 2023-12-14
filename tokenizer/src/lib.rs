#![warn(
    clippy::pedantic,
    clippy::decimal_literal_representation,
    clippy::format_push_string,
    clippy::print_stderr,
    clippy::print_stdout,
    clippy::str_to_string,
    clippy::string_add,
    clippy::string_lit_chars_any,
    clippy::string_to_string,
    clippy::suspicious_xor_used_as_pow,
    clippy::tests_outside_test_module,
    clippy::todo,
    clippy::try_err,
    clippy::undocumented_unsafe_blocks,
    clippy::unimplemented,
    clippy::separated_literal_suffix,
    clippy::unreadable_literal
)]
#![deny(unused_must_use)]

pub use crate::types::{
    Comment, Delimiter, Keyword, LineColumn, Literal, Punct, Span, Token, TokenType,
};
use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::iter::FusedIterator;

mod types;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParseTokenError<'a> {
    InvalidChar(char, &'a str),
    CapsInImproperIdent(&'a str, usize),
    UnderscoreInProper(&'a str, usize),
    UnterminatedStrLit,
    UnterminatedChrLit,
    InvalidEscape(char),
}

impl<'a> Display for ParseTokenError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidChar(c, _) => write!(f, "Invalid char: {c}"),
            Self::CapsInImproperIdent(s, i) => {
                write!(f, "Caps in improper identifier, {s:?}, at pos {i}")
            }
            Self::UnderscoreInProper(s, i) => {
                write!(f, "Underscore in proper identifier, {s:?}, at pos {i}")
            }
            Self::UnterminatedStrLit => write!(f, "No string terminator found!"),
            Self::UnterminatedChrLit => write!(f, "No char terminator found!"),
            Self::InvalidEscape(c) => write!(f, "Invalid escape \\{c}"),
        }
    }
}

impl<'a> std::error::Error for ParseTokenError<'a> {}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SplitTokens<'a> {
    remainder: &'a str,
    original: &'a str,
    line_column: LineColumn,
}

impl<'a> SplitTokens<'a> {
    #[must_use]
    pub(crate) fn new(remainder: &'a str, original: &'a str, line_column: LineColumn) -> Self {
        Self {
            remainder,
            original,
            line_column,
        }
    }

    #[must_use]
    pub fn remainder(&self) -> &str {
        self.remainder
    }

    #[must_use]
    pub fn original(&self) -> &str {
        self.original
    }

    #[must_use]
    pub fn line_column(&self) -> LineColumn {
        self.line_column
    }
}

macro_rules! sp {
    ($char:pat) => {
        ($char, _)
    };
    ($char1:pat, $char2:pat) => {
        ($char1, Some(($char2, _)))
    };
    ($char1:pat, $char2:pat, $char3:pat) => {
        ($char1, Some(($char2, Some($char3))))
    };
}
macro_rules! st {
    ($char:expr, $token_type:expr, $self:expr) => {{
        let char: char = $char;
        let len: usize = char.len_utf8();
        let token_type: crate::types::TokenType = $token_type;
        let (token, remainder): (&str, &str) = $self.remainder.split_at(len);
        Ok((
            Token::new(
                token_type,
                token,
                Span::new(
                    $self.line_column,
                    LineColumn::new($self.line_column.line, $self.line_column.column + 1),
                ),
            ),
            remainder,
        ))
    }};
    ($char1:expr, $char2:expr, $token_type:expr, $self:expr) => {{
        let (char1, char2): (char, char) = ($char1, $char2);
        let len: usize = char1.len_utf8() + char2.len_utf8();
        let token_type: crate::types::TokenType = $token_type;
        let (token, remainder): (&str, &str) = $self.remainder.split_at(len);
        Ok((
            Token::new(
                token_type,
                token,
                Span::new(
                    $self.line_column,
                    LineColumn::new($self.line_column.line, $self.line_column.column + 2),
                ),
            ),
            remainder,
        ))
    }};
}

macro_rules! find_unescaped {
    ($pat:pat, $str:expr) => {{
        let mut escaped = false;
        let mut char_indices: core::str::CharIndices = $str.char_indices();
        let _ = char_indices.next(); // Skip 1
        char_indices
            .find(|(_, c)| match (c, escaped) {
                ('\\', false) => {
                    escaped = true;
                    false
                }
                ($pat, false) => true,
                (_, true) => {
                    escaped = false;
                    false
                }
                (_, false) => false,
            })
            .map(|(i, _)| i)
    }};
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ParseEscapesError(char);

impl<'a> From<ParseEscapesError> for ParseTokenError<'a> {
    fn from(parse_escapes_error: ParseEscapesError) -> Self {
        Self::InvalidEscape(parse_escapes_error.0)
    }
}

/// This function is meant to be used on the content between the terminators of string and character literals.
/// # Errors
/// Returns `Err` if there is an invalid escape in `s`.
pub fn parse_escapes(s: &str) -> Result<Cow<str>, ParseEscapesError> {
    if !s.contains('\\') {
        return Ok(Cow::Borrowed(s));
    }
    let mut escaped = false;
    s.chars()
        .filter_map(|c| match (c, escaped) {
            ('\\', false) => {
                escaped = true;
                None
            }
            (cc, true) => {
                escaped = false;
                match cc {
                    '\\' => Some(Ok('\\')),
                    '\n' => None,
                    'n' => Some(Ok('\n')),
                    't' => Some(Ok('\t')),
                    '0' => Some(Ok('\0')),
                    '"' => Some(Ok('"')),
                    '\'' => Some(Ok('\'')),
                    ccc => Some(Err(ParseEscapesError(ccc))),
                }
            }
            (cc, false) => Some(Ok(cc)),
        })
        .collect()
}

impl<'a> Iterator for SplitTokens<'a> {
    type Item = Result<Token<'a>, ParseTokenError<'a>>;
    #[allow(clippy::too_many_lines)]
    fn next(&mut self) -> Option<Self::Item> {
        self.remainder = self.remainder.trim_matches(|c: char| {
            if c == '\n' {
                self.line_column.column = 0;
                self.line_column.line += 1;
            } else {
                self.line_column.column += usize::from(c.is_whitespace());
            }
            c.is_whitespace()
        });

        let mut chars = self.remainder.chars();
        Some(
            match (chars.next()?, chars.next().map(|c| (c, chars.next()))) {
                // Decimal Integer Literals
                sp!('0'..='9') | sp!('+' | '-', '0'..='9') => {
                    let mut char_indices = self.remainder.char_indices();
                    let _ = char_indices.next(); // Skip 1
                    let (i, lit) = match (
                        char_indices.find(|&(_, c)| !(c.is_ascii_digit() | (c == '_'))),
                        char_indices.next(),
                    ) {
                        (Some((_, '.')), Some((_, '0'..='9'))) => (
                            char_indices
                                .find(|&(_, c)| !(c.is_ascii_digit() | (c == '_')))
                                .map_or(self.remainder.len(), |(i, _)| i),
                            Literal::NonInt,
                        ),
                        (Some((i, _)), _) => (i, Literal::DecInt),
                        (None, _) => (self.remainder.len(), Literal::DecInt),
                    };
                    let (token, remainder) = self.remainder.split_at(i);
                    Ok((
                        Token::new_auto_span(TokenType::Literal(lit), token, self.line_column),
                        remainder,
                    ))
                }
                // Comments
                sp!('/', '/', '/') => {
                    let (token, remainder) = self
                        .remainder
                        .split_at(self.remainder.find('\n').unwrap_or(self.remainder.len()));
                    Ok((
                        Token::new_auto_span(
                            TokenType::Comment(Comment::DocComment),
                            token,
                            self.line_column,
                        ),
                        remainder,
                    ))
                }
                sp!('/', '/') => {
                    let (token, remainder) = self
                        .remainder
                        .split_at(self.remainder.find('\n').unwrap_or(self.remainder.len()));
                    Ok((
                        Token::new_auto_span(
                            TokenType::Comment(Comment::Comment),
                            token,
                            self.line_column,
                        ),
                        remainder,
                    ))
                }
                // Puncts
                sp!(':', '=') => st!(':', '=', TokenType::Punct(Punct::Assign), self),
                sp!('+', '+') => st!('+', '+', TokenType::Punct(Punct::PlusPlus), self),
                sp!('-', '-') => st!('-', '-', TokenType::Punct(Punct::MinusMinus), self),
                sp!('≔') => st!('≔', TokenType::Punct(Punct::Assign), self),
                sp!('+') => st!('+', TokenType::Punct(Punct::Plus), self),
                sp!('/') => st!('/', TokenType::Punct(Punct::Slash), self),
                sp!('*', '*') => st!('*', '*', TokenType::Punct(Punct::StarStar), self),
                sp!('*') => st!('*', TokenType::Punct(Punct::Star), self),
                sp!('%') => st!('%', TokenType::Punct(Punct::Percent), self),
                sp!('^') => st!('^', TokenType::Punct(Punct::Caret), self),
                sp!('&') => st!('&', TokenType::Punct(Punct::And), self),
                sp!('∧') => st!('∧', TokenType::Punct(Punct::And), self),
                sp!('|') => st!('|', TokenType::Punct(Punct::Or), self),
                sp!('∨') => st!('∨', TokenType::Punct(Punct::Or), self),
                sp!('<', '<') => st!('<', '<', TokenType::Punct(Punct::Shl), self),
                sp!('>', '>') => st!('>', '>', TokenType::Punct(Punct::Shr), self),
                sp!('=', '=') => st!('=', '=', TokenType::Punct(Punct::EqEq), self),
                sp!('!') => st!('!', TokenType::Punct(Punct::Not), self),
                sp!('¬') => st!('¬', TokenType::Punct(Punct::Not), self),
                sp!('不') => st!('不', TokenType::Punct(Punct::Not), self),
                sp!('>', '=') => st!('>', '=', TokenType::Punct(Punct::Ge), self),
                sp!('≥') => st!('≥', TokenType::Punct(Punct::Ge), self),
                sp!('<', '=') => st!('<', '=', TokenType::Punct(Punct::Le), self),
                sp!('≤') => st!('≤', TokenType::Punct(Punct::Le), self),
                sp!('>') => st!('>', TokenType::Punct(Punct::Gt), self),
                sp!('<') => st!('<', TokenType::Punct(Punct::Lt), self),
                sp!('@') => st!('@', TokenType::Punct(Punct::At), self),
                sp!('.') => st!('.', TokenType::Punct(Punct::Dot), self),
                sp!(',') => st!(',', TokenType::Punct(Punct::Comma), self),
                sp!(';') => st!(';', TokenType::Punct(Punct::Semi), self),
                sp!(':', ':') => st!(':', ':', TokenType::Punct(Punct::ColonColon), self),
                sp!(':') => st!(':', TokenType::Punct(Punct::Colon), self),
                sp!('-', '>') => st!('-', '>', TokenType::Punct(Punct::RArrow), self),
                sp!('-') => st!('-', TokenType::Punct(Punct::Minus), self),
                sp!('=', '>') => st!('=', '>', TokenType::Punct(Punct::FatArrow), &self),
                sp!('=') => st!('=', TokenType::Punct(Punct::Eq), self),
                sp!('~') => st!('~', TokenType::Punct(Punct::Tilde), self),
                sp!('∀') => st!('∀', TokenType::Punct(Punct::ForAll), self),
                sp!('∃') => st!('∃', TokenType::Punct(Punct::Exists), self),
                // Delimiters
                sp!('{') => st!('{', TokenType::Delimiter(Delimiter::CurlyLeft), self),
                sp!('}') => st!('}', TokenType::Delimiter(Delimiter::CurlyRight), self),
                sp!('[') => st!('[', TokenType::Delimiter(Delimiter::SquareLeft), self),
                sp!(']') => st!(']', TokenType::Delimiter(Delimiter::SquareRight), self),
                sp!('(') => st!('(', TokenType::Delimiter(Delimiter::ParLeft), self),
                sp!(')') => st!(')', TokenType::Delimiter(Delimiter::ParRight), self),
                // String Literals
                sp!('"') => {
                    let Some(index) = find_unescaped!('"', self.remainder) else {
                        return Some(Err(ParseTokenError::UnterminatedStrLit));
                    };
                    Ok((
                        Token::new_auto_span(
                            TokenType::Literal(Literal::String),
                            &self.remainder[..=index],
                            self.line_column,
                        ),
                        &self.remainder[index + 1..],
                    ))
                }
                // Char Literals
                sp!('\'') => {
                    let Some(index) = find_unescaped!('\'', self.remainder) else {
                        return Some(Err(ParseTokenError::UnterminatedChrLit));
                    };
                    Ok((
                        Token::new_auto_span(
                            TokenType::Literal(Literal::Character),
                            &self.remainder[..=index],
                            self.line_column,
                        ),
                        &self.remainder[index + 1..],
                    ))
                }
                // Proper Ident
                (c, _) if c.is_alphabetic() & c.is_uppercase() => {
                    let (token, remainder) = self.remainder.split_at(
                        self.remainder
                            .find(|c: char| !(c.is_alphanumeric() | (c == '_')))
                            .unwrap_or(self.remainder.len()),
                    );
                    if let Some(i) = token.find('_') {
                        return Some(Err(ParseTokenError::UnderscoreInProper(token, i)));
                    }
                    Ok((
                        Token::new_auto_span(TokenType::ProperIdent, token, self.line_column),
                        remainder,
                    ))
                }
                // Ident
                (c, _) if c.is_alphabetic() | (c == '_') => {
                    let (token, remainder) = self.remainder.split_at(
                        self.remainder
                            .find(|c: char| !(c.is_alphanumeric() | (c == '_')))
                            .unwrap_or(self.remainder.len()),
                    );
                    if let Some(i) = token.find(char::is_uppercase) {
                        return Some(Err(ParseTokenError::CapsInImproperIdent(token, i)));
                    }
                    Ok((
                        Token::new_auto_span(
                            match token {
                                "if" => TokenType::Keyword(Keyword::If),
                                "else" => TokenType::Keyword(Keyword::Else),
                                "match" => TokenType::Keyword(Keyword::Match),
                                "while" => TokenType::Keyword(Keyword::While),
                                "loop" => TokenType::Keyword(Keyword::Loop),
                                "true" => TokenType::Keyword(Keyword::True),
                                "false" => TokenType::Keyword(Keyword::False),
                                "let" => TokenType::Keyword(Keyword::Let),
                                "type" => TokenType::Keyword(Keyword::Type),
                                "typeclass" => TokenType::Keyword(Keyword::Typeclass),
                                "ret" => TokenType::Keyword(Keyword::Ret),
                                "gen" => TokenType::Keyword(Keyword::Gen),
                                "where" => TokenType::Keyword(Keyword::Where),
                                "miguel" => TokenType::Keyword(Keyword::Miguel),
                                "kyasig" => TokenType::Keyword(Keyword::Kyasig),
                                "claim" => TokenType::Keyword(Keyword::Claim),
                                "cardinality" => TokenType::Keyword(Keyword::Cardinality),
                                "bytes" => TokenType::Keyword(Keyword::Bytes),
                                "bits" => TokenType::Keyword(Keyword::Bits),
                                // Underscore Punct
                                "_" => TokenType::Punct(Punct::Underscore),
                                _ => TokenType::Ident,
                            },
                            token,
                            self.line_column,
                        ),
                        remainder,
                    ))
                }
                (c, _) => Err(ParseTokenError::InvalidChar(
                    c,
                    &self.remainder[..c.len_utf8()],
                )),
            }
            .map(|(token, remainder)| {
                (self.line_column, self.remainder) = (token.span.end, remainder);
                token
            }),
        )
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.remainder.len()))
    }
}

impl<'a> FusedIterator for SplitTokens<'a> {}

#[must_use]
pub fn split_tokens(string: &str) -> SplitTokens {
    SplitTokens::new(string, string, LineColumn::new(0, 0))
}
