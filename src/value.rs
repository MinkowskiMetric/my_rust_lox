use crate::LoxError;
use std::{
    convert::{Infallible, TryFrom},
    fmt,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    True,
    False,
    Number(f64),
    String(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::True => write!(f, "true"),
            Value::False => write!(f, "false"),
            Value::Number(num) => write!(f, "{}", num),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        match b {
            true => Self::True,
            false => Self::False,
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = Infallible;

    fn try_from(v: Value) -> Result<Self, Self::Error> {
        match v {
            Value::Nil => Ok(false),
            Value::True => Ok(true),
            Value::False => Ok(false),
            Value::Number(num) => Ok(num != 0.0),
            Value::String(s) => Ok(!s.is_empty()),
        }
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Self::Number(f)
    }
}

impl TryFrom<Value> for f64 {
    type Error = LoxError;

    fn try_from(v: Value) -> Result<f64, Self::Error> {
        match v {
            Value::Number(num) => Ok(num),
            v => Err(LoxError::ValueError(v, "number".into())),
        }
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}
