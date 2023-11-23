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
