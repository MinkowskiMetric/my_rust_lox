use crate::{EnvironmentRef, Interpreter, LoxResult, ResolvedStatement, Value};
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

#[derive(Debug, Clone)]
struct ScriptCallable {
    parameters: Vec<String>,
    body: ResolvedStatement,
    env: EnvironmentRef,
}

impl Callable for ScriptCallable {
    fn arity(&self) -> usize {
        self.parameters.len()
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: &[Value]) -> LoxResult<Value> {
        assert_eq!(self.parameters.len(), arguments.len());

        let mut frame = interpreter.create_function_frame(&self.env);
        for i in 0..self.parameters.len() {
            frame.declare_variable(&self.parameters[i], arguments[i].clone())?;
        }

        frame.call_function(&self.body)
    }
}

impl fmt::Display for ScriptCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str("function")
    }
}

pub fn make_script_function(
    parameters: &[String],
    body: &ResolvedStatement,
    env: &EnvironmentRef,
) -> LoxResult<Value> {
    Ok(Value::from(ScriptCallable {
        parameters: parameters.to_vec(),
        body: body.clone(),
        env: env.clone(),
    }))
}
