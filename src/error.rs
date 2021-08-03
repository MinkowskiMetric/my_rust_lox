use crate::Position;
use std::{error, fmt, io};

#[derive(Debug)]
pub enum LoxError<'a> {
    InvalidCharacter(char, Position<'a>),
    TokenizationError(Vec<LoxError<'a>>),
    UnexpectedEndOfFile(Position<'a>),
    UnknownEscapeSequence(char, Position<'a>),
    InvalidNumber(std::num::ParseFloatError, Position<'a>),
    IoError(io::Error),
}

impl From<io::Error> for LoxError<'_> {
    fn from(e: io::Error) -> Self {
        Self::IoError(e)
    }
}

impl error::Error for LoxError<'_> {

}

impl fmt::Display for LoxError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::InvalidCharacter(c, pos) => write!(f, "Invalid character '{}' at {}", c, pos),
            _ => todo!(),
        }
    }
}

pub type LoxResult<'a, T> = Result<T, LoxError<'a>>;
