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
    Or,
    And,
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
            Self::Or => write!(f, "|"),
            Self::And => write!(f, "&"),
        }
    }
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

impl Display for Delimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CurlyLeft => write!(f, "{{"),
            Self::CurlyRight => write!(f, "}}"),
            Self::SquareLeft => write!(f, "["),
            Self::SquareRight => write!(f, "]"),
            Self::ParLeft => write!(f, "("),
            Self::ParRight => write!(f, ")"),
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

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Match => write!(f, "match"),
            Self::While => write!(f, "while"),
            Self::Loop => write!(f, "loop"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Let => write!(f, "let"),
            Self::Type => write!(f, "type"),
            Self::Return => write!(f, "return"),
            Self::Gen => write!(f, "gen"),
            Self::Func => write!(f, "func"),
        }
    }
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

impl Display for Arrow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RArrow => write!(f, "->"),
            Self::FatArrow => write!(f, "=>"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Dot {
    Dot,
}

impl Display for Dot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dot => write!(f, "."),
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

#[inline]
fn symbol_token<E>(
    chars: (char, Option<char>),
    token_kind: TokenKind,
    s: &str,
) -> Option<Result<(Token, &str), E>> {
    let len = chars.0.len_utf8() + chars.1.map(char::len_utf8).unwrap_or(0);
    Some(Ok((Token(token_kind, &s[..len]), &s[len..])))
}

impl<'a> Iterator for SplitTokens<'a> {
    type Item = Result<Token<'a>, ParseTokenError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let trimmed = self.remainder.trim();

        if trimmed.is_empty() {
            return None;
        }

        let mut chars = trimmed.chars();
        match (chars.next()?, chars.next().map(|c| (c, chars.next()))) {
            ('0'..='9', _) | ('+' | '-', Some(('0'..='9', _))) => {
                let (token, remainder) = trimmed.split_at(
                    trimmed
                        .char_indices()
                        .skip(1)
                        .find(|(_, f)| !(f.is_ascii_digit()))
                        .map(|(i, _)| i)
                        .unwrap_or(trimmed.len()),
                );
                match token.parse() {
                    Ok(i) => Some(Ok((Token(TokenKind::Number(i), token), remainder))),
                    Err(e) => Some(Err(ParseTokenError::ParseIntError(e, token))),
                }
            }
            ('-', Some(('>', _))) => {
                symbol_token(('-', Some('>')), TokenKind::Arrow(Arrow::RArrow), trimmed)
            }
            ('=', Some(('>', _))) => {
                symbol_token(('=', Some('>')), TokenKind::Arrow(Arrow::FatArrow), trimmed)
            }
            ('/', Some(('/', Some('/')))) => {
                let (token, remainder) =
                    trimmed.split_at(trimmed.find('\n').unwrap_or(trimmed.len()));
                Some(Ok((
                    Token(TokenKind::Comment(Comment::DocComment), token),
                    remainder,
                )))
            }
            ('/', Some(('/', _))) => {
                let (token, remainder) =
                    trimmed.split_at(trimmed.find('\n').unwrap_or(trimmed.len()));
                Some(Ok((
                    Token(TokenKind::Comment(Comment::Comment), token),
                    remainder,
                )))
            }
            ('+', _) => symbol_token(
                ('+', None),
                TokenKind::Operator(BinaryOperator::Add),
                trimmed,
            ),
            ('-', _) => symbol_token(
                ('-', None),
                TokenKind::Operator(BinaryOperator::Sub),
                trimmed,
            ),
            ('*', Some(('*', _))) => symbol_token(
                ('*', Some('*')),
                TokenKind::Operator(BinaryOperator::Pow),
                trimmed,
            ),
            ('*', _) => symbol_token(
                ('*', None),
                TokenKind::Operator(BinaryOperator::Mul),
                trimmed,
            ),
            ('/', _) => symbol_token(
                ('/', None),
                TokenKind::Operator(BinaryOperator::Div),
                trimmed,
            ),
            ('%', _) => symbol_token(
                ('%', None),
                TokenKind::Operator(BinaryOperator::Mod),
                trimmed,
            ),
            (':', Some(('=', _))) => symbol_token(
                (':', Some('=')),
                TokenKind::Operator(BinaryOperator::Assign),
                trimmed,
            ),
            ('=', Some(('=', _))) => symbol_token(
                ('=', Some('=')),
                TokenKind::Operator(BinaryOperator::Eq),
                trimmed,
            ),
            ('!', Some(('=', _))) => symbol_token(
                ('!', Some('=')),
                TokenKind::Operator(BinaryOperator::Ne),
                trimmed,
            ),
            ('>', Some(('=', _))) => symbol_token(
                ('>', Some('=')),
                TokenKind::Operator(BinaryOperator::Ge),
                trimmed,
            ),
            ('<', Some(('=', _))) => symbol_token(
                ('<', Some('=')),
                TokenKind::Operator(BinaryOperator::Le),
                trimmed,
            ),
            ('>', _) => symbol_token(
                ('>', None),
                TokenKind::Operator(BinaryOperator::Gt),
                trimmed,
            ),
            ('<', _) => symbol_token(
                ('<', None),
                TokenKind::Operator(BinaryOperator::Lt),
                trimmed,
            ),
            ('|', _) => symbol_token(
                ('|', None),
                TokenKind::Operator(BinaryOperator::Or),
                trimmed,
            ),
            ('&', _) => symbol_token(
                ('&', None),
                TokenKind::Operator(BinaryOperator::And),
                trimmed,
            ),
            ('{', _) => symbol_token(
                ('{', None),
                TokenKind::Delimiter(Delimiter::CurlyLeft),
                trimmed,
            ),
            ('}', _) => symbol_token(
                ('}', None),
                TokenKind::Delimiter(Delimiter::CurlyRight),
                trimmed,
            ),
            ('[', _) => symbol_token(
                ('[', None),
                TokenKind::Delimiter(Delimiter::SquareLeft),
                trimmed,
            ),
            (']', _) => symbol_token(
                (']', None),
                TokenKind::Delimiter(Delimiter::SquareRight),
                trimmed,
            ),
            ('(', _) => symbol_token(
                ('(', None),
                TokenKind::Delimiter(Delimiter::ParLeft),
                trimmed,
            ),
            (')', _) => symbol_token(
                (')', None),
                TokenKind::Delimiter(Delimiter::ParRight),
                trimmed,
            ),
            (',', _) => symbol_token((',', None), TokenKind::Separator(Separator::Comma), trimmed),
            (':', _) => symbol_token((':', None), TokenKind::Separator(Separator::Colon), trimmed),
            (';', _) => symbol_token((';', None), TokenKind::Separator(Separator::Semi), trimmed),
            ('.', _) => symbol_token(('.', None), TokenKind::Dot(Dot::Dot), trimmed),
            ('"', _) => {
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
                    return Some(Err(ParseTokenError::UnterminatedString));
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
                    Ok(string) => Some(Ok((
                        Token(TokenKind::String(string), &trimmed[..=index]),
                        &trimmed[index + 1..],
                    ))),
                    Err(e) => Some(Err(e)),
                }
            }
            (c, _) if c.is_alphabetic() | (c == '_') => {
                let mut is_macro = false;
                let (token, remainder) = trimmed.split_at(
                    if c.is_uppercase() {
                        trimmed.find(|c: char| !(c.is_alphanumeric() | (c == '_')))
                    } else {
                        trimmed.find(|c| match (c, is_macro) {
                            (_, true) => true,
                            ('!', false) => {
                                is_macro = true;
                                false
                            }
                            (cc, _) => !(cc.is_alphanumeric() | (cc == '_')),
                        })
                    }
                    .unwrap_or(trimmed.len()),
                );
                Some(Ok((
                    Token(
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
                            _ if is_macro => TokenKind::MacroName,
                            _ if c.is_uppercase() => TokenKind::TypeName,
                            _ => TokenKind::Name,
                        },
                        token,
                    ),
                    remainder,
                )))
            }
            (c, _) => Some(Err(ParseTokenError::InvalidChar(
                c,
                &trimmed[..c.len_utf8()],
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
