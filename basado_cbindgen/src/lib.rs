mod types;

pub use types::*;

#[repr(C)]
pub enum Result<T, E> {
    Ok(T),
    Err(E),
}

impl<T, E> From<std::result::Result<T, E>> for Result<T, E> {
    fn from(result: std::result::Result<T, E>) -> Self {
        match result {
            Ok(t) => Result::Ok(t),
            Err(e) => Result::Err(e),
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParseTokenError {
    InvalidChar(char, StrRef),
    CapsInImproperIdent(StrRef, usize),
    UnderscoreInProper(StrRef, usize),
    UnterminatedString,
    InvalidEscape(char),
}

impl<'a> From<tokenizer::ParseTokenError<'a>> for ParseTokenError {
    fn from(parse_token_error: tokenizer::ParseTokenError) -> Self {
        match parse_token_error {
            tokenizer::ParseTokenError::InvalidChar(c, s) => Self::InvalidChar(c, s.into()),
            tokenizer::ParseTokenError::CapsInImproperIdent(s, u) => {
                Self::CapsInImproperIdent(s.into(), u)
            }
            tokenizer::ParseTokenError::UnderscoreInProper(s, u) => {
                Self::UnderscoreInProper(s.into(), u)
            }
            tokenizer::ParseTokenError::UnterminatedString => Self::UnterminatedString,
            tokenizer::ParseTokenError::InvalidEscape(c) => Self::InvalidEscape(c),
        }
    }
}
