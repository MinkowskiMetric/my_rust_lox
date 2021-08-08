use crate::LoxError;
use std::{convert::TryFrom, fmt};

#[derive(Clone, Debug, PartialEq)]
pub struct Nil;

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

impl From<Nil> for Value {
    fn from(_: Nil) -> Value {
        Value::Nil
    }
}

impl TryFrom<Value> for Nil {
    type Error = LoxError;

    fn try_from(v: Value) -> Result<Self, Self::Error> {
        match v {
            Value::Nil => Ok(Self),
            v => Err(LoxError::ValueError(v, "nil".into())),
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

impl From<Value> for bool {
    fn from(v: Value) -> Self {
        match v {
            Value::Nil => false,
            Value::False => false,
            _ => true,
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

    fn try_from(v: Value) -> Result<Self, Self::Error> {
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

impl TryFrom<Value> for String {
    type Error = LoxError;

    fn try_from(v: Value) -> Result<Self, Self::Error> {
        match v {
            Value::String(s) => Ok(s),
            v => Err(LoxError::ValueError(v, "string".into())),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a str {
    type Error = LoxError;

    fn try_from(v: &'a Value) -> Result<Self, Self::Error> {
        match v {
            Value::String(s) => Ok(s),
            v => Err(LoxError::ValueError(v.clone(), "string".into())),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn check_value_equal_rust<T>(v: Value, t: T) -> bool
    where
        T: Into<Value>,
    {
        v == t.into()
    }

    fn check_rust_equal_value<T>(check_value: T, v: Value) -> bool
    where
        T: TryFrom<Value> + PartialEq,
    {
        match T::try_from(v) {
            Ok(v) => v == check_value,
            _ => false,
        }
    }

    fn check_invalid_type<T>(check_value: Value, check_type_str: &str) -> bool
    where
        T: TryFrom<Value, Error = LoxError>,
    {
        match T::try_from(check_value.clone()) {
            Err(LoxError::ValueError(v, type_str)) => {
                v == check_value && type_str == check_type_str
            }
            _ => false,
        }
    }

    #[test]
    fn nil_test() {
        assert!(check_value_equal_rust(Value::Nil, Nil));
        assert!(check_rust_equal_value(Nil, Value::Nil));

        assert!(check_invalid_type::<Nil>(Value::True, "nil"));
        assert!(check_invalid_type::<Nil>(Value::False, "nil"));
        assert!(check_invalid_type::<Nil>(Value::Number(19.0), "nil"));
        assert!(check_invalid_type::<Nil>(
            Value::String("boo".into()),
            "nil"
        ));
    }

    #[test]
    fn bool_test() {
        // First check that bool properly converts into Value
        assert!(check_value_equal_rust(Value::False, false));
        assert!(check_value_equal_rust(Value::True, true));
        assert!(check_rust_equal_value(false, Value::False));
        assert!(check_rust_equal_value(true, Value::True));

        // Now check truthiness
        assert!(check_rust_equal_value(false, Value::Nil));
        assert!(check_rust_equal_value(false, Value::False));
        assert!(check_rust_equal_value(true, Value::True));
        assert!(check_rust_equal_value(true, Value::Number(0.0)));
        assert!(check_rust_equal_value(true, Value::Number(10.0)));
        assert!(check_rust_equal_value(true, Value::String("".to_string())));
        assert!(check_rust_equal_value(
            true,
            Value::String("true".to_string())
        ));
        assert!(check_rust_equal_value(
            true,
            Value::String("false".to_string())
        ));
    }

    #[test]
    fn number_test() {
        assert!(check_value_equal_rust(Value::Number(0.0), 0.0));
        assert!(check_value_equal_rust(Value::Number(-17.0), -17.0));
        assert!(check_rust_equal_value(0.0, Value::Number(0.0)));
        assert!(check_rust_equal_value(-17.0, Value::Number(-17.0)));

        assert!(check_invalid_type::<f64>(Value::Nil, "number"));
        assert!(check_invalid_type::<f64>(Value::True, "number"));
        assert!(check_invalid_type::<f64>(Value::False, "number"));
        assert!(check_invalid_type::<f64>(
            Value::String("boo".into()),
            "number"
        ));
    }

    #[test]
    fn string_test() {
        assert!(check_value_equal_rust(
            Value::String("".to_string()),
            "".to_string()
        ));
        assert!(check_value_equal_rust(
            Value::String("hello".to_string()),
            "hello".to_string()
        ));
        assert!(check_value_equal_rust(
            Value::String("world".to_string()),
            "world".to_string()
        ));
        assert!(check_rust_equal_value(
            "".to_string(),
            Value::String("".to_string())
        ));
        assert!(check_rust_equal_value(
            "hello".to_string(),
            Value::String("hello".to_string())
        ));
        assert!(check_rust_equal_value(
            "world".to_string(),
            Value::String("world".to_string())
        ));

        assert!(check_invalid_type::<String>(Value::Nil, "string"));
        assert!(check_invalid_type::<String>(Value::True, "string"));
        assert!(check_invalid_type::<String>(Value::False, "string"));
        assert!(check_invalid_type::<String>(Value::Number(10.0), "string"));
    }

    #[test]
    fn display_test() {
        assert_eq!(Value::Nil.to_string(), "nil");
        assert_eq!(Value::True.to_string(), "true");
        assert_eq!(Value::False.to_string(), "false");
        assert_eq!(Value::Number(0.0).to_string(), "0");
        assert_eq!(Value::Number(-17.0).to_string(), "-17");
        assert_eq!(Value::String("hello".into()).to_string(), "hello");
    }
}
