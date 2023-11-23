use std::mem::ManuallyDrop;

#[repr(C)]
#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Str {
    pub ptr: *mut u8,
    pub len: usize,
    pub capacity: usize,
}

impl Clone for Str {
    fn clone(&self) -> Self {
        Self::from(unsafe { std::slice::from_raw_parts(self.ptr, self.len) }.to_vec())
    }
}

impl Drop for Str {
    fn drop(&mut self) {
        drop(unsafe { Vec::<u8>::from_raw_parts(self.ptr, self.len, self.capacity) })
    }
}

impl Str {
    fn from(s: Vec<u8>) -> Self {
        let mut x = ManuallyDrop::new(s);
        Self {
            ptr: x.as_mut_ptr(),
            len: x.len(),
            capacity: x.capacity(),
        }
    }
}
#[no_mangle]
pub extern "C" fn drop_str(str: Str) {
    drop(str)
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct StrRef {
    pub ptr: *const u8,
    pub len: usize,
}

impl From<&[u8]> for StrRef {
    fn from(s: &[u8]) -> Self {
        Self {
            ptr: s.as_ptr(),
            len: s.len(),
        }
    }
}

impl From<&str> for StrRef {
    fn from(s: &str) -> Self {
        Self::from(s.as_bytes())
    }
}

#[repr(C)]
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl From<tokenizer::Span> for Span {
    fn from(span: tokenizer::Span) -> Self {
        Self {
            start: span.start.into(),
            end: span.end.into(),
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl From<tokenizer::LineColumn> for LineColumn {
    fn from(line_column: tokenizer::LineColumn) -> Self {
        Self {
            line: line_column.line,
            column: line_column.column,
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token {
    pub token_kind: TokenKind,
    pub s: StrRef,
    pub span: Span,
}

impl<'a> From<tokenizer::Token<'a>> for Token {
    fn from(token: tokenizer::Token<'a>) -> Self {
        Self {
            token_kind: token.token_kind.into(),
            s: token.s.into(),
            span: token.span.into(),
        }
    }
}

#[repr(C)]
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

impl From<tokenizer::TokenKind> for TokenKind {
    fn from(token_kind: tokenizer::TokenKind) -> Self {
        match token_kind {
            tokenizer::TokenKind::Keyword(keyword) => Self::Keyword(keyword.into()),
            tokenizer::TokenKind::Ident => Self::Ident,
            tokenizer::TokenKind::ProperIdent => Self::ProperIdent,
            tokenizer::TokenKind::Literal(literal) => Self::Literal(literal.into()),
            tokenizer::TokenKind::Punct(punct) => Self::Punct(punct.into()),
            tokenizer::TokenKind::Delimiter(delimiter) => Self::Delimiter(delimiter.into()),
            tokenizer::TokenKind::Comment(comment) => Self::Comment(comment.into()),
        }
    }
}

#[repr(C)]
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
    Typeclass,
    Ret,
    Gen,
    Where,
    Miguel,
    Kyasig,
}

impl From<tokenizer::Keyword> for Keyword {
    fn from(keyword: tokenizer::Keyword) -> Self {
        match keyword {
            tokenizer::Keyword::If => Self::If,
            tokenizer::Keyword::Else => Self::Else,
            tokenizer::Keyword::Match => Self::Match,
            tokenizer::Keyword::While => Self::While,
            tokenizer::Keyword::Loop => Self::Loop,
            tokenizer::Keyword::True => Self::True,
            tokenizer::Keyword::False => Self::False,
            tokenizer::Keyword::Let => Self::Let,
            tokenizer::Keyword::Type => Self::Type,
            tokenizer::Keyword::Typeclass => Self::Typeclass,
            tokenizer::Keyword::Ret => Self::Ret,
            tokenizer::Keyword::Gen => Self::Gen,
            tokenizer::Keyword::Where => Self::Where,
            tokenizer::Keyword::Miguel => Self::Miguel,
            tokenizer::Keyword::Kyasig => Self::Kyasig,
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Literal {
    Character(char),
    String(Str),
    Number,
}

impl From<tokenizer::Literal> for Literal {
    fn from(literal: tokenizer::Literal) -> Self {
        match literal {
            tokenizer::Literal::Character(character) => Self::Character(character),
            tokenizer::Literal::String(string) => Self::String(Str::from(string.into_bytes())),
            tokenizer::Literal::Number => Self::Number,
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Punct {
    Assign,
    PlusPlus,
    MinusMinus,
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
    Shl,
    Shr,
    Eq,
    EqEq,
    Gt,
    Lt,
    Ge,
    Le,
    At,
    Underscore,
    Dot,
    Comma,
    Semi,
    Colon,
    ColonColon,
    RArrow,
    FatArrow,
    Tilde,
    ForAll,
    Exists,
}

impl From<tokenizer::Punct> for Punct {
    fn from(punct: tokenizer::Punct) -> Self {
        match punct {
            tokenizer::Punct::Assign => Self::Assign,
            tokenizer::Punct::PlusPlus => Self::PlusPlus,
            tokenizer::Punct::MinusMinus => Self::MinusMinus,
            tokenizer::Punct::Plus => Self::Plus,
            tokenizer::Punct::Minus => Self::Minus,
            tokenizer::Punct::Star => Self::Star,
            tokenizer::Punct::Slash => Self::Slash,
            tokenizer::Punct::StarStar => Self::StarStar,
            tokenizer::Punct::Percent => Self::Percent,
            tokenizer::Punct::Caret => Self::Caret,
            tokenizer::Punct::Not => Self::Not,
            tokenizer::Punct::And => Self::And,
            tokenizer::Punct::Or => Self::Or,
            tokenizer::Punct::Shl => Self::Shl,
            tokenizer::Punct::Shr => Self::Shr,
            tokenizer::Punct::Eq => Self::Eq,
            tokenizer::Punct::EqEq => Self::EqEq,
            tokenizer::Punct::Gt => Self::Gt,
            tokenizer::Punct::Lt => Self::Lt,
            tokenizer::Punct::Ge => Self::Ge,
            tokenizer::Punct::Le => Self::Le,
            tokenizer::Punct::At => Self::At,
            tokenizer::Punct::Underscore => Self::Underscore,
            tokenizer::Punct::Dot => Self::Dot,
            tokenizer::Punct::Comma => Self::Comma,
            tokenizer::Punct::Semi => Self::Semi,
            tokenizer::Punct::Colon => Self::Colon,
            tokenizer::Punct::ColonColon => Self::ColonColon,
            tokenizer::Punct::RArrow => Self::RArrow,
            tokenizer::Punct::FatArrow => Self::FatArrow,
            tokenizer::Punct::Tilde => Self::Tilde,
            tokenizer::Punct::ForAll => Self::ForAll,
            tokenizer::Punct::Exists => Self::Exists,
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Delimiter {
    CurlyLeft,
    CurlyRight,
    SquareLeft,
    SquareRight,
    ParLeft,
    ParRight,
}

impl From<tokenizer::Delimiter> for Delimiter {
    fn from(delimiter: tokenizer::Delimiter) -> Self {
        match delimiter {
            tokenizer::Delimiter::CurlyLeft => Self::CurlyLeft,
            tokenizer::Delimiter::CurlyRight => Self::CurlyRight,
            tokenizer::Delimiter::SquareLeft => Self::SquareLeft,
            tokenizer::Delimiter::SquareRight => Self::SquareRight,
            tokenizer::Delimiter::ParLeft => Self::ParLeft,
            tokenizer::Delimiter::ParRight => Self::ParRight,
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Comment {
    Comment,
    DocComment,
}

impl From<tokenizer::Comment> for Comment {
    fn from(comment: tokenizer::Comment) -> Self {
        match comment {
            tokenizer::Comment::Comment => Self::Comment,
            tokenizer::Comment::DocComment => Self::DocComment,
        }
    }
}
