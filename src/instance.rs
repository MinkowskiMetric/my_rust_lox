use crate::{Class, LoxError, LoxResult, Value};
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

#[derive(Clone, Debug)]
pub struct Instance {
    class: Rc<Class>,
    properties: RefCell<HashMap<String, Value>>,
}

pub type InstanceRef = Rc<Instance>;

impl Instance {
    pub fn new(class: Rc<Class>) -> InstanceRef {
        Rc::new(Self {
            class,
            properties: RefCell::new(HashMap::new()),
        })
    }

    pub fn get(&self, name: &str) -> LoxResult<Value> {
        match self
            .properties
            .borrow()
            .get(name)
            .or_else(|| self.class.lookup_method(name))
        {
            Some(v) => Ok(v.clone()),
            None => Err(LoxError::UnknownVariable(name.to_string())),
        }
    }

    pub fn set(&self, name: &str, value: &Value) -> LoxResult<()> {
        self.properties
            .borrow_mut()
            .insert(name.to_string(), value.clone());
        Ok(())
    }
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "instance of {}", self.class)
    }
}
