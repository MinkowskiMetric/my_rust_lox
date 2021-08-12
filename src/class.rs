use crate::{Callable, Instance, Interpreter, LoxResult, Value};
use std::{fmt, rc::Rc};

#[derive(Clone, Debug)]
pub struct Class {
    name: String,
}

impl Class {
    pub fn new(name: &str) -> Rc<Self> {
        Rc::new(Self {
            name: name.to_string(),
        })
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "class({})", self.name)
    }
}

impl Callable for Class {
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
