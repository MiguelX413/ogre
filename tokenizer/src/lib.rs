pub use crate::types::{
    Comment, Delimiter, Keyword, LineColumn, Literal, Punct, Span, Token, TokenKind,
};
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

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
    line_column: LineColumn,
    original: &'a str,
}

impl<'a> SplitTokens<'a> {
    fn new(string: &str) -> SplitTokens {
        SplitTokens {
            remainder: string,
            line_column: LineColumn::default(),
            original: string,
        }
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
    ($char:expr, $token_kind:expr, $self:expr) => {{
        let char: char = $char;
        let len: usize = char.len_utf8();
        let token_kind: crate::types::TokenKind = $token_kind;
        let (token, remainder): (&str, &str) = $self.remainder.split_at(len);
        Ok((
            Token::new(
                token_kind,
                token,
                Span::new(
                    $self.line_column,
                    LineColumn::new($self.line_column.line, $self.line_column.column + 1),
                ),
            ),
            remainder,
        ))
    }};
    ($char1:expr, $char2:expr, $token_kind:expr, $self:expr) => {{
        let (char1, char2): (char, char) = ($char1, $char2);
        let len: usize = char1.len_utf8() + char2.len_utf8();
        let token_kind: crate::types::TokenKind = $token_kind;
        let (token, remainder): (&str, &str) = $self.remainder.split_at(len);
        Ok((
            Token::new(
                token_kind,
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

macro_rules! terminator_finder {
    ($pat:pat, $str:expr) => {{
        let mut escaped = false;
        $str.char_indices()
            .skip(1)
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

/// This function is meant to be used on the content between the terminators of string and character literals
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
                // Number Literals
                sp!('0'..='9') | sp!('+' | '-', '0'..='9') => {
                    let (token, remainder) = self.remainder.split_at(
                        self.remainder
                            .char_indices()
                            .skip(1)
                            .find(|&(_, c)| !(c.is_ascii_digit() | (c == '_')))
                            .map(|(i, _)| i)
                            .unwrap_or(self.remainder.len()),
                    );
                    Ok((
                        Token::new_auto_span(
                            TokenKind::Literal(Literal::Number),
                            token,
                            self.line_column,
                        ),
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
                            TokenKind::Comment(Comment::DocComment),
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
                            TokenKind::Comment(Comment::Comment),
                            token,
                            self.line_column,
                        ),
                        remainder,
                    ))
                }
                // Puncts
                sp!(':', '=') => st!(':', '=', TokenKind::Punct(Punct::Assign), self),
                sp!('+', '+') => st!('+', '+', TokenKind::Punct(Punct::PlusPlus), self),
                sp!('-', '-') => st!('-', '-', TokenKind::Punct(Punct::MinusMinus), self),
                sp!('≔') => st!('≔', TokenKind::Punct(Punct::Assign), self),
                sp!('+') => st!('+', TokenKind::Punct(Punct::Plus), self),
                sp!('/') => st!('/', TokenKind::Punct(Punct::Slash), self),
                sp!('*', '*') => st!('*', '*', TokenKind::Punct(Punct::StarStar), self),
                sp!('*') => st!('*', TokenKind::Punct(Punct::Star), self),
                sp!('%') => st!('%', TokenKind::Punct(Punct::Percent), self),
                sp!('^') => st!('^', TokenKind::Punct(Punct::Caret), self),
                sp!('&') => st!('&', TokenKind::Punct(Punct::And), self),
                sp!('∧') => st!('∧', TokenKind::Punct(Punct::And), self),
                sp!('|') => st!('|', TokenKind::Punct(Punct::Or), self),
                sp!('∨') => st!('∨', TokenKind::Punct(Punct::Or), self),
                sp!('<', '<') => st!('<', '<', TokenKind::Punct(Punct::Shl), self),
                sp!('>', '>') => st!('>', '>', TokenKind::Punct(Punct::Shr), self),
                sp!('=', '=') => st!('=', '=', TokenKind::Punct(Punct::EqEq), self),
                sp!('!') => st!('!', TokenKind::Punct(Punct::Not), self),
                sp!('¬') => st!('¬', TokenKind::Punct(Punct::Not), self),
                sp!('不') => st!('不', TokenKind::Punct(Punct::Not), self),
                sp!('>', '=') => st!('>', '=', TokenKind::Punct(Punct::Ge), self),
                sp!('≥') => st!('≥', TokenKind::Punct(Punct::Ge), self),
                sp!('<', '=') => st!('<', '=', TokenKind::Punct(Punct::Le), self),
                sp!('≤') => st!('≤', TokenKind::Punct(Punct::Le), self),
                sp!('>') => st!('>', TokenKind::Punct(Punct::Gt), self),
                sp!('<') => st!('<', TokenKind::Punct(Punct::Lt), self),
                sp!('@') => st!('@', TokenKind::Punct(Punct::At), self),
                sp!('.') => st!('.', TokenKind::Punct(Punct::Dot), self),
                sp!(',') => st!(',', TokenKind::Punct(Punct::Comma), self),
                sp!(';') => st!(';', TokenKind::Punct(Punct::Semi), self),
                sp!(':', ':') => st!(':', ':', TokenKind::Punct(Punct::ColonColon), self),
                sp!(':') => st!(':', TokenKind::Punct(Punct::Colon), self),
                sp!('-', '>') => st!('-', '>', TokenKind::Punct(Punct::RArrow), self),
                sp!('-') => st!('-', TokenKind::Punct(Punct::Minus), self),
                sp!('=', '>') => st!('=', '>', TokenKind::Punct(Punct::FatArrow), &self),
                sp!('=') => st!('=', TokenKind::Punct(Punct::Eq), self),
                sp!('~') => st!('~', TokenKind::Punct(Punct::Tilde), self),
                sp!('∀') => st!('∀', TokenKind::Punct(Punct::ForAll), self),
                sp!('∃') => st!('∃', TokenKind::Punct(Punct::Exists), self),
                // Delimiters
                sp!('{') => st!('{', TokenKind::Delimiter(Delimiter::CurlyLeft), self),
                sp!('}') => st!('}', TokenKind::Delimiter(Delimiter::CurlyRight), self),
                sp!('[') => st!('[', TokenKind::Delimiter(Delimiter::SquareLeft), self),
                sp!(']') => st!(']', TokenKind::Delimiter(Delimiter::SquareRight), self),
                sp!('(') => st!('(', TokenKind::Delimiter(Delimiter::ParLeft), self),
                sp!(')') => st!(')', TokenKind::Delimiter(Delimiter::ParRight), self),
                // String Literals
                sp!('"') => {
                    let Some(index) = terminator_finder!('"', self.remainder) else {
                        return Some(Err(ParseTokenError::UnterminatedStrLit));
                    };
                    Ok((
                        Token::new_auto_span(
                            TokenKind::Literal(Literal::String),
                            &self.remainder[..=index],
                            self.line_column,
                        ),
                        &self.remainder[index + 1..],
                    ))
                }
                // Char Literals
                sp!('\'') => {
                    let Some(index) = terminator_finder!('\'', self.remainder) else {
                        return Some(Err(ParseTokenError::UnterminatedChrLit));
                    };
                    Ok((
                        Token::new_auto_span(
                            TokenKind::Literal(Literal::Character),
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
                        Token::new_auto_span(TokenKind::ProperIdent, token, self.line_column),
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
                                "if" => TokenKind::Keyword(Keyword::If),
                                "else" => TokenKind::Keyword(Keyword::Else),
                                "match" => TokenKind::Keyword(Keyword::Match),
                                "while" => TokenKind::Keyword(Keyword::While),
                                "loop" => TokenKind::Keyword(Keyword::Loop),
                                "true" => TokenKind::Keyword(Keyword::True),
                                "false" => TokenKind::Keyword(Keyword::False),
                                "let" => TokenKind::Keyword(Keyword::Let),
                                "type" => TokenKind::Keyword(Keyword::Type),
                                "typeclass" => TokenKind::Keyword(Keyword::Typeclass),
                                "ret" => TokenKind::Keyword(Keyword::Ret),
                                "gen" => TokenKind::Keyword(Keyword::Gen),
                                "where" => TokenKind::Keyword(Keyword::Where),
                                "miguel" => TokenKind::Keyword(Keyword::Miguel),
                                "kyasig" => TokenKind::Keyword(Keyword::Kyasig),
                                // Underscore Punct
                                "_" => TokenKind::Punct(Punct::Underscore),
                                _ => TokenKind::Ident,
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

pub fn split_tokens(string: &str) -> SplitTokens {
    SplitTokens::new(string)
}
