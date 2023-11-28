#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Span {
    pub fn new(start: LineColumn, end: LineColumn) -> Self {
        Self { start, end }
    }
}

impl From<(LineColumn, LineColumn)> for Span {
    fn from((start, end): (LineColumn, LineColumn)) -> Self {
        Self { start, end }
    }
}

impl From<Span> for (LineColumn, LineColumn) {
    fn from(span: Span) -> Self {
        (span.start, span.end)
    }
}

#[derive(Copy, Clone, Default, Debug, Eq, Hash, PartialEq)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl LineColumn {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl From<(usize, usize)> for LineColumn {
    fn from((line, column): (usize, usize)) -> Self {
        Self { line, column }
    }
}

impl From<LineColumn> for (usize, usize) {
    fn from(line_column: LineColumn) -> Self {
        (line_column.line, line_column.column)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token<'a> {
    pub token_kind: TokenKind,
    pub s: &'a str,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(token_kind: TokenKind, s: &'a str, span: Span) -> Self {
        Self {
            token_kind,
            s,
            span,
        }
    }

    pub fn new_auto_span(token_kind: TokenKind, s: &'a str, mut line_column: LineColumn) -> Self {
        Self {
            token_kind,
            s,
            span: Span::new(line_column, {
                s.chars().for_each(|c| {
                    if c == '\n' {
                        line_column.column = 0;
                        line_column.line += 1;
                    } else {
                        line_column.column += 1;
                    }
                });
                line_column
            }),
        }
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
    Typeclass,
    Ret,
    Gen,
    Where,
    Miguel,
    Kyasig,
    Claim,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Literal {
    Character,
    String,
    Number,
}

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
