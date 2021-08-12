use crate::{
    Environment, EnvironmentRef, FuncType, Instance, Interpreter, LoxResult, ResolvedStatement,
    Value,
};
use std::{cell::RefCell, fmt, rc::Rc};

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

    pub fn bind(self: Rc<Self>, instance: Rc<Instance>) -> Rc<Self> {
        let mut new_environment = Environment::new(Some(self.env.clone()));
        new_environment
            .declare("this", instance.into())
            .expect("Can't fail in empty environment");

        let new_environment = Rc::new(RefCell::new(new_environment));

        Self::new(
            self.func_type,
            &self.parameters,
            &self.body,
            &new_environment,
        )
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

        let ret = frame.call_function(&self.body)?;

        Ok(if self.func_type() == FuncType::Initializer {
            self.env
                .borrow()
                .get("this", 0)
                .expect("Initializer must have this")
        } else {
            ret
        })
    }
}

impl fmt::Display for ScriptCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str("function")
    }
}
