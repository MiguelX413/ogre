use crate::types::defs::{
    Comment, Delimiter, Keyword, Literal, ParseTokenError, Punct, Token, TokenKind,
};

mod types;

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
    ($char:expr, $token_kind:expr, $remainder:expr) => {{
        const CHAR: char = $char;
        const LEN: usize = CHAR.len_utf8();
        let token_kind: crate::types::defs::TokenKind = $token_kind;
        let remainder: &str = $remainder;
        Some(Ok((
            crate::types::defs::Token::new(token_kind, &remainder[..LEN]),
            &remainder[LEN..],
        )))
    }};
    ($char1:expr, $char2:expr, $token_kind:expr, $remainder:expr) => {{
        const CHAR1: char = $char1;
        const CHAR2: char = $char2;
        const LEN: usize = CHAR1.len_utf8() + CHAR2.len_utf8();
        let token_kind: crate::types::defs::TokenKind = $token_kind;
        let remainder: &str = $remainder;
        Some(Ok((
            crate::types::defs::Token::new(token_kind, &remainder[..LEN]),
            &remainder[LEN..],
        )))
    }};
}

impl<'a> Iterator for SplitTokens<'a> {
    type Item = Result<Token<'a>, ParseTokenError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.remainder = self.remainder.trim();

        let mut chars = self.remainder.chars();
        match (chars.next()?, chars.next().map(|c| (c, chars.next()))) {
            ('0'..='9', _) | ('+' | '-', Some(('0'..='9', _))) => {
                let (token, remainder) = self.remainder.split_at(
                    self.remainder
                        .char_indices()
                        .skip(1)
                        .find(|(_, f)| !(f.is_ascii_digit()))
                        .map(|(i, _)| i)
                        .unwrap_or(self.remainder.len()),
                );
                Some(Ok((
                    Token::new(TokenKind::Literal(Literal::Number), token),
                    remainder,
                )))
            }
            sp!('-', '>') => st!('-', '>', TokenKind::Punct(Punct::RArrow), self.remainder),
            sp!('=', '>') => st!('=', '>', TokenKind::Punct(Punct::FatArrow), &self.remainder),
            sp!('/', '/', '/') => {
                let (token, remainder) = self
                    .remainder
                    .split_at(self.remainder.find('\n').unwrap_or(self.remainder.len()));
                Some(Ok((
                    Token::new(TokenKind::Comment(Comment::DocComment), token),
                    remainder,
                )))
            }
            sp!('/', '/') => {
                let (token, remainder) = self
                    .remainder
                    .split_at(self.remainder.find('\n').unwrap_or(self.remainder.len()));
                Some(Ok((
                    Token::new(TokenKind::Comment(Comment::Comment), token),
                    remainder,
                )))
            }
            sp!('+') => st!('+', TokenKind::Punct(Punct::Plus), self.remainder),
            sp!('-') => st!('-', TokenKind::Punct(Punct::Minus), self.remainder),
            sp!('*', '*') => st!('*', '*', TokenKind::Punct(Punct::StarStar), self.remainder),
            sp!('*') => st!('*', TokenKind::Punct(Punct::Star), self.remainder),
            sp!('/') => st!('/', TokenKind::Punct(Punct::Slash), self.remainder),
            sp!('%') => st!('%', TokenKind::Punct(Punct::Percent), self.remainder),
            sp!('^') => st!('^', TokenKind::Punct(Punct::Caret), self.remainder),
            sp!(':', '=') => st!(':', '=', TokenKind::Punct(Punct::Assign), self.remainder),
            sp!('=', '=') => st!('=', '=', TokenKind::Punct(Punct::Eq), self.remainder),
            sp!('!', '=') => st!('!', '=', TokenKind::Punct(Punct::Ne), self.remainder),
            sp!('!') => st!('!', TokenKind::Punct(Punct::Not), self.remainder),
            sp!('>', '=') => st!('>', '=', TokenKind::Punct(Punct::Ge), self.remainder),
            sp!('<', '=') => st!('<', '=', TokenKind::Punct(Punct::Le), self.remainder),
            sp!('>') => st!('>', TokenKind::Punct(Punct::Gt), self.remainder),
            sp!('<') => st!('<', TokenKind::Punct(Punct::Lt), self.remainder),
            sp!('|') => st!('|', TokenKind::Punct(Punct::Or), self.remainder),
            sp!('&') => st!('&', TokenKind::Punct(Punct::And), self.remainder),
            sp!('{') => st!(
                '{',
                TokenKind::Delimiter(Delimiter::CurlyLeft),
                self.remainder
            ),
            sp!('}') => st!(
                '}',
                TokenKind::Delimiter(Delimiter::CurlyRight),
                self.remainder
            ),
            sp!('[') => st!(
                '[',
                TokenKind::Delimiter(Delimiter::SquareLeft),
                self.remainder
            ),
            sp!(']') => st!(
                ']',
                TokenKind::Delimiter(Delimiter::SquareRight),
                self.remainder
            ),
            sp!('(') => st!(
                '(',
                TokenKind::Delimiter(Delimiter::ParLeft),
                self.remainder
            ),
            sp!(')') => st!(
                ')',
                TokenKind::Delimiter(Delimiter::ParRight),
                self.remainder
            ),
            sp!(',') => st!(',', TokenKind::Punct(Punct::Comma), self.remainder),
            sp!(':') => st!(':', TokenKind::Punct(Punct::Colon), self.remainder),
            sp!(';') => st!(';', TokenKind::Punct(Punct::Semi), self.remainder),
            sp!('.') => st!('.', TokenKind::Punct(Punct::Dot), self.remainder),
            ('"', _) => {
                let mut escaped = false;
                let Some(index) = self
                    .remainder
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
                    return Some(Err(ParseTokenError::UnterminatedString));
                };
                let mut escaped = false;
                match self.remainder[1..index]
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
                                '\n' => None,
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
                    Ok(string) => Some(Ok((
                        Token::new(
                            TokenKind::Literal(Literal::String(string)),
                            &self.remainder[..=index],
                        ),
                        &self.remainder[index + 1..],
                    ))),
                    Err(e) => Some(Err(e)),
                }
            }
            (c, _) if c.is_alphabetic() | (c == '_') => {
                let (token, remainder) = self.remainder.split_at(
                    self.remainder
                        .find(|c: char| !(c.is_alphanumeric() | (c == '_')))
                        .unwrap_or(self.remainder.len()),
                );
                if let Some(i) = token.find(char::is_uppercase) {
                    return Some(Err(ParseTokenError::CapsInImproperIdent(token, i)));
                }
                Some(Ok((
                    Token::new(
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
                            "return" => TokenKind::Keyword(Keyword::Return),
                            "gen" => TokenKind::Keyword(Keyword::Gen),
                            "func" => TokenKind::Keyword(Keyword::Func),
                            _ => TokenKind::Ident,
                        },
                        token,
                    ),
                    remainder,
                )))
            }
            (c, _) => Some(Err(ParseTokenError::InvalidChar(
                c,
                &self.remainder[..c.len_utf8()],
            ))),
        }
        .map(|f| {
            f.map(|(token, remainder)| {
                self.remainder = remainder;
                token
            })
        })
    }
}

pub fn split_tokens(string: &str) -> SplitTokens {
    SplitTokens::new(string)
}

pub fn main() {
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
        r#"let my_string := "lol\"test";
let xd := 2;
let multi_line_str := "xd\
sus";"#,
    ]
    .into_iter()
    .for_each(|string| {
        println!(
            "{string:?}: {:?}",
            split_tokens(string).collect::<Result<Vec<_>, _>>()
        )
    });
}
