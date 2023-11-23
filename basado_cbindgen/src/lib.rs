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

#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct ValidatedStrRef<'a> {
    s: &'a str,
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Utf8Error {
    utf8error: core::str::Utf8Error,
}

impl From<core::str::Utf8Error> for Utf8Error {
    fn from(utf8error: std::str::Utf8Error) -> Self {
        Self { utf8error }
    }
}

#[no_mangle]
pub extern "C" fn validate_str<'a>(str_ref: StrRef) -> Result<ValidatedStrRef<'a>, Utf8Error> {
    fn inner<'a>(str_ref: StrRef) -> std::result::Result<ValidatedStrRef<'a>, std::str::Utf8Error> {
        Ok(ValidatedStrRef {
            s: std::str::from_utf8(unsafe {
                std::slice::from_raw_parts(str_ref.ptr, str_ref.len)
            })?,
        })
    }
    inner(str_ref).map_err(Utf8Error::from).into()
}
