use crate::{BValue, BValueType, Position, Token, Value};
use std::{error, fmt, io};

#[derive(Debug)]
pub enum LoxError {
    InvalidCharacter(char, Position),
    TokenizationError(Vec<LoxError>),
    UnexpectedEndOfFile(Position),
    UnknownEscapeSequence(char, Position),
    InvalidNumber(std::num::ParseFloatError, Position),
    IoError(io::Error),
    UnexpectedToken(Token, Position),
    IncompleteExpression(Position),
    ValueError(Value, String),
    BValueTypeError(BValue, BValueType),
    MissingIdentifier(Position),
    UnknownVariable(String),
}

impl From<io::Error> for LoxError {
    fn from(e: io::Error) -> Self {
        Self::IoError(e)
    }
}

impl From<std::convert::Infallible> for LoxError {
    fn from(_: std::convert::Infallible) -> Self {
        panic!("Infallible?")
    }
}

impl error::Error for LoxError {}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::InvalidCharacter(c, pos) => write!(f, "Invalid character '{}' at {}", c, pos),
            Self::UnexpectedEndOfFile(pos) => write!(f, "Unexpected end of file at {}", pos),
            Self::UnknownEscapeSequence(c, pos) => {
                write!(f, "Unknown escape sequence \\{} at {}", c, pos)
            }
            Self::InvalidNumber(err, pos) => write!(f, "Number error {} at {}", err, pos),
            Self::IoError(err) => write!(f, "{}", err),
            Self::UnexpectedToken(token, pos) => write!(f, "Unexpected token {} at {}", token, pos),
            Self::IncompleteExpression(pos) => write!(f, "Incomplete expression at {}", pos),
            Self::ValueError(value, expected) => {
                write!(f, "Found value {}, expected {}", value, expected)
            }
            Self::BValueTypeError(value, expected_type) => {
                write!(f, "Found value {}, expected {}", value, expected_type)
            }
            Self::MissingIdentifier(pos) => write!(f, "Identifier missing at {}", pos),
            Self::UnknownVariable(name) => write!(f, "Unknown variable '{}'", name),

            Self::TokenizationError(errs) => {
                for err in errs {
                    writeln!(f, "{}", err)?;
                }
                Ok(())
            }
        }
    }
}

pub type LoxResult<T> = Result<T, LoxError>;

pub enum UnwindableLoxError {
    Error(LoxError),
    Return(Value),
}

pub type UnwindableLoxResult<T> = Result<T, UnwindableLoxError>;

impl UnwindableLoxError {
    pub fn return_value(v: Value) -> Self {
        Self::Return(v)
    }
}

impl From<LoxError> for UnwindableLoxError {
    fn from(e: LoxError) -> Self {
        Self::Error(e)
    }
}
