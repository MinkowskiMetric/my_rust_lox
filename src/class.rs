use crate::{Callable, FuncType, Instance, Interpreter, LoxResult, Value};
use std::{collections::HashMap, fmt, rc::Rc};

#[derive(Clone, Debug)]
pub struct Class {
    name: String,
    methods: HashMap<String, Value>,
}

impl Class {
    pub fn new(name: &str, methods: HashMap<String, Value>) -> Rc<Self> {
        Rc::new(Self {
            name: name.to_string(),
            methods,
        })
    }

    pub fn lookup_method(&self, name: &str) -> Option<&Value> {
        self.methods.get(name)
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "class({})", self.name)
    }
}

impl Callable for Class {
    fn func_type(&self) -> FuncType {
        FuncType::Function
    }
    fn arity(&self) -> usize {
        0
    }

    fn call(
        self: Rc<Self>,
        _interpreter: &mut Interpreter,
        _arguments: &[Value],
    ) -> LoxResult<Value> {
        let ret = Instance::new(self);
        Ok(ret.into())
    }
}
