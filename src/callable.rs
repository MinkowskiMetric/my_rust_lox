use crate::{Interpreter, LoxResult, Value};
use std::fmt;

pub trait Callable: fmt::Debug + fmt::Display {
    fn arity(&self) -> usize;

    fn call(&self, interpreter: &mut Interpreter, arguments: &[Value]) -> LoxResult<Value>;
}

pub type CallableReference<'a> = &'a dyn Callable;

#[derive(Clone)]
struct NativeCallable<F: Fn(&mut Interpreter, &[Value]) -> LoxResult<Value>> {
    f: F,
    arity: usize,
}

impl<F: Fn(&mut Interpreter, &[Value]) -> LoxResult<Value>> NativeCallable<F> {
    pub fn new(f: F, arity: usize) -> Self {
        Self { f, arity }
    }
}

impl<F: Fn(&mut Interpreter, &[Value]) -> LoxResult<Value>> Callable for NativeCallable<F> {
    fn arity(&self) -> usize {
        self.arity
    }
    fn call(&self, interpreter: &mut Interpreter, arguments: &[Value]) -> LoxResult<Value> {
        (self.f)(interpreter, arguments)
    }
}

impl<F: Fn(&mut Interpreter, &[Value]) -> LoxResult<Value>> fmt::Debug for NativeCallable<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str("<NATIVE FUNCTION>")
    }
}

impl<F: Fn(&mut Interpreter, &[Value]) -> LoxResult<Value>> fmt::Display for NativeCallable<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str("<NATIVE FUNCTION>")
    }
}

pub fn make_native_function<F: 'static + Fn(&mut Interpreter, &[Value]) -> LoxResult<Value>>(
    arity: usize,
    f: F,
) -> Value {
    NativeCallable::new(f, arity).into()
}
