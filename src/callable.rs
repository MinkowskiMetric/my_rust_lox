use crate::{EnvironmentRef, FuncType, Interpreter, LoxResult, ResolvedStatement, Value};
use std::{fmt, rc::Rc};

pub trait Callable: fmt::Debug + fmt::Display {
    fn func_type(&self) -> FuncType;
    fn arity(&self) -> usize;

    fn call(self: Rc<Self>, interpreter: &mut Interpreter, arguments: &[Value])
        -> LoxResult<Value>;
}

#[derive(Clone)]
pub struct NativeCallable<F: Fn(&mut Interpreter, &[Value]) -> LoxResult<Value>> {
    f: F,
    arity: usize,
}

impl<F: Fn(&mut Interpreter, &[Value]) -> LoxResult<Value>> NativeCallable<F> {
    pub fn new(f: F, arity: usize) -> Rc<Self> {
        Rc::new(Self { f, arity })
    }
}

impl<F: Fn(&mut Interpreter, &[Value]) -> LoxResult<Value>> Callable for NativeCallable<F> {
    fn func_type(&self) -> FuncType {
        FuncType::Function
    }

    fn arity(&self) -> usize {
        self.arity
    }
    fn call(
        self: Rc<Self>,
        interpreter: &mut Interpreter,
        arguments: &[Value],
    ) -> LoxResult<Value> {
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

#[derive(Debug, Clone)]
pub struct ScriptCallable {
    func_type: FuncType,
    parameters: Vec<String>,
    body: ResolvedStatement,
    env: EnvironmentRef,
}

impl ScriptCallable {
    pub fn new(
        func_type: FuncType,
        parameters: &[String],
        body: &ResolvedStatement,
        env: &EnvironmentRef,
    ) -> Rc<Self> {
        Rc::new(Self {
            func_type,
            parameters: parameters.to_vec(),
            body: body.clone(),
            env: env.clone(),
        })
    }
}

impl Callable for ScriptCallable {
    fn func_type(&self) -> FuncType {
        self.func_type
    }

    fn arity(&self) -> usize {
        self.parameters.len()
    }

    fn call(
        self: Rc<Self>,
        interpreter: &mut Interpreter,
        arguments: &[Value],
    ) -> LoxResult<Value> {
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
