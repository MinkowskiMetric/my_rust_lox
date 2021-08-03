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

impl error::Error for LoxError<'_> {}

impl fmt::Display for LoxError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::InvalidCharacter(c, pos) => write!(f, "Invalid character '{}' at {}", c, pos),
            Self::UnexpectedEndOfFile(pos) => write!(f, "Unexpected end of file at {}", pos),
            Self::UnknownEscapeSequence(c, pos) => write!(f, "Unknown escape sequence \\{} at {}", c, pos),
            Self::InvalidNumber(err, pos) => write!(f, "Number error {} at {}", err, pos),
            Self::IoError(err) => write!(f, "{}", err),

            Self::TokenizationError(errs) => {
                for err in errs {
                    writeln!(f, "{}", err)?;
                }
                Ok(())
            }
            _ => todo!(),
        }
    }
}

pub type LoxResult<'a, T> = Result<T, LoxError<'a>>;
