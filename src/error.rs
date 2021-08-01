use crate::Position;
use std::io;

#[derive(Debug)]
pub enum LoxError<'a> {
    InvalidCharacter(char, Position<'a>),
    TokenizationError(Vec<LoxError<'a>>),
    IoError(io::Error),
}

impl From<io::Error> for LoxError<'_> {
    fn from(e: io::Error) -> Self {
        Self::IoError(e)
    }
}

pub type LoxResult<'a, T> = Result<T, LoxError<'a>>;
