use crate::{Interpreter, LoxResult, Value};
use std::fmt;

pub trait Callable: fmt::Debug + fmt::Display {
    fn get_arity(&self) -> usize;

    fn call(&self, interpreter: &mut Interpreter, arguments: &[Value]) -> LoxResult<Value>;
}
