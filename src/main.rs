use std::fmt::{Display, Formatter};
use std::str::FromStr;

const _: () = assert!(std::mem::size_of::<*const u8>() == std::mem::size_of::<usize>());
fn substr_index(s: &str, substr: &str) -> usize {
    substr.as_ptr() as usize - s.as_ptr() as usize
}

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

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl LineColumn {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Span {
    pub fn new(start: LineColumn, end: LineColumn) -> Self {
        Self { start, end }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token<'a> {
    pub token_kind: TokenKind,
    pub str: &'a str,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(token_kind: TokenKind, str: &'a str, span: Span) -> Self {
        Self {
            token_kind,
            str,
            span,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TokenKind {
    Operator(BinaryOperator),
    Delimiter(Delimiter),
    Separator(Separator),
    Keyword(Keyword),
    Name,
    Type,
    Macro,
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
    line_column: LineColumn,
}

impl<'a> SplitTokens<'a> {
    pub fn new(string: &str) -> SplitTokens {
        SplitTokens {
            remainder: string,
            original: string,
            line_column: LineColumn::new(0, 0),
        }
    }
}

#[inline]
fn symbol_token<E>(
    chars: (char, Option<char>),
    token_kind: TokenKind,
    s: &str,
    line_column: LineColumn,
) -> Option<Result<(Token, &str), E>> {
    let len = chars.0.len_utf8() + chars.1.map(char::len_utf8).unwrap_or(0);
    Some(Ok((
        Token::new(
            token_kind,
            &s[..len],
            Span::new(
                line_column,
                LineColumn::new(line_column.line, line_column.column + len),
            ),
        ),
        &s[len..],
    )))
}

impl<'a> Iterator for SplitTokens<'a> {
    type Item = Result<Token<'a>, ParseTokenError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let trimmed = self.remainder.trim_start_matches(|c: char| {
            if c == '\n' {
                self.line_column.line += 1;
                self.line_column.column = 0;
            } else {
                self.line_column.column += usize::from(c.is_whitespace());
            }
            c.is_whitespace()
        });

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
                    Ok(i) => Some(Ok((
                        Token::new(
                            TokenKind::Number(i),
                            token,
                            Span::new(
                                self.line_column,
                                LineColumn::new(
                                    self.line_column.line,
                                    self.line_column.column + token.len(),
                                ),
                            ),
                        ),
                        remainder,
                    ))),
                    Err(e) => Some(Err(ParseTokenError::ParseIntError(e, token))),
                }
            }
            ('-', Some(('>', _))) => symbol_token(
                ('-', Some('>')),
                TokenKind::Arrow(Arrow::RArrow),
                trimmed,
                self.line_column,
            ),
            ('=', Some(('>', _))) => symbol_token(
                ('=', Some('>')),
                TokenKind::Arrow(Arrow::FatArrow),
                trimmed,
                self.line_column,
            ),
            ('/', Some(('/', Some('/')))) => {
                let (token, remainder) =
                    trimmed.split_at(trimmed.find('\n').unwrap_or(trimmed.len()));
                Some(Ok((
                    Token::new(
                        TokenKind::Comment(Comment::DocComment),
                        token,
                        Span::new(
                            self.line_column,
                            LineColumn::new(
                                self.line_column.line,
                                self.line_column.column + token.len(),
                            ),
                        ),
                    ),
                    remainder,
                )))
            }
            ('/', Some(('/', _))) => {
                let (token, remainder) =
                    trimmed.split_at(trimmed.find('\n').unwrap_or(trimmed.len()));
                Some(Ok((
                    Token::new(
                        TokenKind::Comment(Comment::Comment),
                        token,
                        Span::new(
                            self.line_column,
                            LineColumn::new(
                                self.line_column.line,
                                self.line_column.column + token.len(),
                            ),
                        ),
                    ),
                    remainder,
                )))
            }
            ('+', _) => symbol_token(
                ('+', None),
                TokenKind::Operator(BinaryOperator::Add),
                trimmed,
                self.line_column,
            ),
            ('-', _) => symbol_token(
                ('-', None),
                TokenKind::Operator(BinaryOperator::Sub),
                trimmed,
                self.line_column,
            ),
            ('*', Some(('*', _))) => symbol_token(
                ('*', Some('*')),
                TokenKind::Operator(BinaryOperator::Pow),
                trimmed,
                self.line_column,
            ),
            ('*', _) => symbol_token(
                ('*', None),
                TokenKind::Operator(BinaryOperator::Mul),
                trimmed,
                self.line_column,
            ),
            ('/', _) => symbol_token(
                ('/', None),
                TokenKind::Operator(BinaryOperator::Div),
                trimmed,
                self.line_column,
            ),
            ('%', _) => symbol_token(
                ('%', None),
                TokenKind::Operator(BinaryOperator::Mod),
                trimmed,
                self.line_column,
            ),
            (':', Some(('=', _))) => symbol_token(
                (':', Some('=')),
                TokenKind::Operator(BinaryOperator::Assign),
                trimmed,
                self.line_column,
            ),
            ('=', Some(('=', _))) => symbol_token(
                ('=', Some('=')),
                TokenKind::Operator(BinaryOperator::Eq),
                trimmed,
                self.line_column,
            ),
            ('!', Some(('=', _))) => symbol_token(
                ('!', Some('=')),
                TokenKind::Operator(BinaryOperator::Ne),
                trimmed,
                self.line_column,
            ),
            ('>', Some(('=', _))) => symbol_token(
                ('>', Some('=')),
                TokenKind::Operator(BinaryOperator::Ge),
                trimmed,
                self.line_column,
            ),
            ('<', Some(('=', _))) => symbol_token(
                ('<', Some('=')),
                TokenKind::Operator(BinaryOperator::Le),
                trimmed,
                self.line_column,
            ),
            ('>', _) => symbol_token(
                ('>', None),
                TokenKind::Operator(BinaryOperator::Gt),
                trimmed,
                self.line_column,
            ),
            ('<', _) => symbol_token(
                ('<', None),
                TokenKind::Operator(BinaryOperator::Lt),
                trimmed,
                self.line_column,
            ),
            ('|', _) => symbol_token(
                ('|', None),
                TokenKind::Operator(BinaryOperator::Or),
                trimmed,
                self.line_column,
            ),
            ('&', _) => symbol_token(
                ('&', None),
                TokenKind::Operator(BinaryOperator::And),
                trimmed,
                self.line_column,
            ),
            ('{', _) => symbol_token(
                ('{', None),
                TokenKind::Delimiter(Delimiter::CurlyLeft),
                trimmed,
                self.line_column,
            ),
            ('}', _) => symbol_token(
                ('}', None),
                TokenKind::Delimiter(Delimiter::CurlyRight),
                trimmed,
                self.line_column,
            ),
            ('[', _) => symbol_token(
                ('[', None),
                TokenKind::Delimiter(Delimiter::SquareLeft),
                trimmed,
                self.line_column,
            ),
            (']', _) => symbol_token(
                (']', None),
                TokenKind::Delimiter(Delimiter::SquareRight),
                trimmed,
                self.line_column,
            ),
            ('(', _) => symbol_token(
                ('(', None),
                TokenKind::Delimiter(Delimiter::ParLeft),
                trimmed,
                self.line_column,
            ),
            (')', _) => symbol_token(
                (')', None),
                TokenKind::Delimiter(Delimiter::ParRight),
                trimmed,
                self.line_column,
            ),
            (',', _) => symbol_token(
                (',', None),
                TokenKind::Separator(Separator::Comma),
                trimmed,
                self.line_column,
            ),
            (':', _) => symbol_token(
                (':', None),
                TokenKind::Separator(Separator::Colon),
                trimmed,
                self.line_column,
            ),
            (';', _) => symbol_token(
                (';', None),
                TokenKind::Separator(Separator::Semi),
                trimmed,
                self.line_column,
            ),
            ('.', _) => symbol_token(
                ('.', None),
                TokenKind::Dot(Dot::Dot),
                trimmed,
                self.line_column,
            ),
            ('"', _) => {
                let mut line_column = self.line_column;
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
                    .map(|r| {
                        r.map(|c| {
                            match c {
                                '\n' => {
                                    line_column.line += 1;
                                    line_column.column = 0;
                                }
                                _ => line_column.column += 1,
                            };
                            c
                        })
                    })
                    .collect::<Result<_, _>>()
                {
                    Ok(string) => Some(Ok((
                        Token::new(
                            TokenKind::String(string),
                            &trimmed[..=index],
                            Span::new(self.line_column, line_column),
                        ),
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
                            _ if is_macro => TokenKind::Macro,
                            _ if c.is_uppercase() => TokenKind::Type,
                            _ => TokenKind::Name,
                        },
                        token,
                        Span::new(
                            self.line_column,
                            token.chars().fold(self.line_column, |acc, f| match f {
                                '\n' => LineColumn::new(acc.line + 1, 0),
                                _ => LineColumn::new(acc.line, acc.column + 1),
                            }),
                        ),
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
                self.line_column = token.span.end;
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
            "{string:?}: {:?}\n",
            split_tokens(string).collect::<Result<Vec<_>, _>>()
        )
    });
}
