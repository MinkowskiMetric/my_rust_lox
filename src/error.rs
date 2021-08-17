use crate::{BValue, BValueType, Position, SimpleToken, Token, Value};
use std::{error, fmt, io};

#[derive(Debug)]
pub enum LoxError {
    MultipleErrors(Vec<LoxError>),
    InvalidCharacter(char, Position),
    UnexpectedEndOfFile(Position),
    UnknownEscapeSequence(char, Position),
    InvalidNumber(std::num::ParseFloatError, Position),
    IoError(io::Error),
    IncompleteExpression(Position),
    ValueError(Value, String),
    BValueTypeError(BValue, BValueType),
    MissingIdentifier(Position),
    UnknownVariable(String),
    UseVariableInInitializer(Position, String),
    DuplicateVariable(String, Position, Position),
    ThisOutsideMethod(Position),
    SuperOutsideMethod(Position),
    ReturnFromInitializer(Position),
    IncorrectArgumentCount,

    ExpectedToken(Position, SimpleToken),
    UnexpectedToken(Position, Token),
    ExpectedIdentifier(Position),
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
            Self::IncompleteExpression(pos) => write!(f, "Incomplete expression at {}", pos),
            Self::ValueError(value, expected) => {
                write!(f, "Found value {}, expected {}", value, expected)
            }
            Self::BValueTypeError(value, expected_type) => {
                write!(f, "Found value {}, expected {}", value, expected_type)
            }
            Self::MissingIdentifier(pos) => write!(f, "Identifier missing at {}", pos),
            Self::UnknownVariable(name) => write!(f, "Unknown variable '{}'", name),
            Self::UseVariableInInitializer(name, pos) => {
                write!(f, "Used variable {} in initializer at {}", name, pos)
            }
            Self::DuplicateVariable(name, original_pos, new_pos) => write!(
                f,
                "Duplicate variable {} at {} originally declared at {}",
                name, new_pos, original_pos
            ),
            Self::ThisOutsideMethod(pos) => write!(f, "this used outside method at {}", pos),
            Self::SuperOutsideMethod(pos) => write!(f, "super used outside method at {}", pos),
            Self::ReturnFromInitializer(pos) => write!(f, "Return from initializer at {}", pos),
            Self::IncorrectArgumentCount => write!(f, "Incorrect argument count"),

            Self::MultipleErrors(errs) => {
                for err in errs {
                    writeln!(f, "{}", err)?;
                }
                Ok(())
            }

            other => write!(f, "{:?}", other),
        }
    }
}

pub type LoxResult<T> = Result<T, LoxError>;

#[derive(Debug)]
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
