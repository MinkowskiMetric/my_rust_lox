use crate::{BValue, Callable, InstanceRef, LoxError};
use std::{
    convert::{TryFrom, TryInto},
    fmt,
    rc::Rc,
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Nil;

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct CallableHolder(Rc<dyn Callable>);

impl fmt::Display for CallableHolder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl PartialEq<CallableHolder> for CallableHolder {
    fn eq(&self, _rhs: &Self) -> bool {
        todo!("I haven't implemented equality for callable yet - I probably can if I need to. This goes away when I get an object model and a garbage collector")
    }
}

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct InstanceHolder(InstanceRef);

impl fmt::Display for InstanceHolder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl PartialEq<InstanceHolder> for InstanceHolder {
    fn eq(&self, _rhs: &Self) -> bool {
        todo!("I haven't implemented equality for instance yet - I probably can if I need to. This goes away when I get an object model and a garbage collector")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    BValue(BValue),
    String(String),
    Callable(CallableHolder),
    Instance(InstanceHolder),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Value::BValue(val) => val.fmt(f),
            Value::String(s) => write!(f, "{}", s),
            Value::Callable(func) => write!(f, "{}", func),
            Value::Instance(func) => write!(f, "{}", func),
        }
    }
}

impl From<Nil> for Value {
    fn from(_: Nil) -> Value {
        Value::BValue(BValue::nil_val())
    }
}

impl TryFrom<Value> for Nil {
    type Error = LoxError;

    fn try_from(v: Value) -> Result<Self, Self::Error> {
        match v {
            Value::BValue(bval) if bval.is_nil() => Ok(Self),
            v => Err(LoxError::ValueError(v, "nil".into())),
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::BValue(BValue::from_bool(b))
    }
}

impl From<Value> for bool {
    fn from(v: Value) -> Self {
        match v {
            Value::BValue(bv) => bv.to_bool(),
            _ => true,
        }
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Self::BValue(BValue::from(f))
    }
}

impl TryFrom<Value> for f64 {
    type Error = LoxError;

    fn try_from(v: Value) -> Result<Self, Self::Error> {
        match v {
            Value::BValue(bval) => bval
                .try_into()
                .map_err(|_| LoxError::ValueError(v, "number".into())),
            v => Err(LoxError::ValueError(v, "number".into())),
        }
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Self::String(s.to_string())
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

impl<T: 'static + Callable> From<Rc<T>> for Value {
    fn from(t: Rc<T>) -> Self {
        Self::Callable(CallableHolder(t))
    }
}

impl TryFrom<Value> for Rc<dyn Callable> {
    type Error = LoxError;

    fn try_from(v: Value) -> Result<Self, Self::Error> {
        match v {
            Value::Callable(callable) => Ok(callable.0),
            v => Err(LoxError::ValueError(v, "callable".into())),
        }
    }
}

impl From<InstanceRef> for Value {
    fn from(t: InstanceRef) -> Self {
        Self::Instance(InstanceHolder(t))
    }
}

impl TryFrom<Value> for InstanceRef {
    type Error = LoxError;

    fn try_from(v: Value) -> Result<Self, Self::Error> {
        match v {
            Value::Instance(instance) => Ok(instance.0),
            v => Err(LoxError::ValueError(v, "instance".into())),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a InstanceRef {
    type Error = LoxError;

    fn try_from(v: &'a Value) -> Result<Self, Self::Error> {
        match v {
            Value::Instance(instance) => Ok(&instance.0),
            v => Err(LoxError::ValueError(v.clone(), "instance".into())),
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

    fn check_rust_equal_value<E, T>(check_value: T, v: Value) -> bool
    where
        E: fmt::Debug,
        T: TryFrom<Value, Error = E> + PartialEq + fmt::Debug,
    {
        match T::try_from(v) {
            Ok(v) => v == check_value,
            Err(e) => panic!("{:?} {:?}", e, check_value), //false,
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
        assert!(check_value_equal_rust(Value::from(Nil), Nil));
        assert!(check_rust_equal_value(Nil, Value::from(Nil)));

        assert!(check_invalid_type::<Nil>(Value::from(true), "nil"));
        assert!(check_invalid_type::<Nil>(Value::from(false), "nil"));
        assert!(check_invalid_type::<Nil>(Value::from(19.0), "nil"));
        assert!(check_invalid_type::<Nil>(
            Value::String("boo".into()),
            "nil"
        ));
    }

    #[test]
    fn bool_test() {
        // First check that bool properly converts into Value
        assert!(check_value_equal_rust(Value::from(false), false));
        assert!(check_value_equal_rust(Value::from(true), true));
        assert!(check_rust_equal_value(false, Value::from(false)));
        assert!(check_rust_equal_value(true, Value::from(true)));

        // Now check truthiness
        assert!(check_rust_equal_value(false, Value::from(Nil)));
        assert!(check_rust_equal_value(false, Value::from(false)));
        assert!(check_rust_equal_value(true, Value::from(true)));
        assert!(check_rust_equal_value(true, Value::from(0.0)));
        assert!(check_rust_equal_value(true, Value::from(10.0)));
        assert!(check_rust_equal_value(true, Value::from("".to_string())));
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
        assert!(check_value_equal_rust(Value::from(0.0), 0.0));
        assert!(check_value_equal_rust(Value::from(-17.0), -17.0));
        assert!(check_rust_equal_value(0.0, Value::from(0.0)));
        assert!(check_rust_equal_value(-17.0, Value::from(-17.0)));

        assert!(
            check_invalid_type::<f64>(Value::from(Nil), "number"),
            "{:?}",
            f64::try_from(Value::from(Nil))
        );
        assert!(check_invalid_type::<f64>(Value::from(true), "number"));
        assert!(check_invalid_type::<f64>(Value::from(false), "number"));
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

        assert!(check_invalid_type::<String>(Value::from(Nil), "string"));
        assert!(check_invalid_type::<String>(Value::from(true), "string"));
        assert!(check_invalid_type::<String>(Value::from(false), "string"));
        assert!(check_invalid_type::<String>(Value::from(10.0), "string"));
    }

    #[test]
    fn display_test() {
        assert_eq!(Value::from(Nil).to_string(), "nil");
        assert_eq!(Value::from(true).to_string(), "true");
        assert_eq!(Value::from(false).to_string(), "false");
        assert_eq!(Value::from(0.0).to_string(), "0");
        assert_eq!(Value::from(-17.0).to_string(), "-17");
        assert_eq!(Value::String("hello".into()).to_string(), "hello");
    }
}
