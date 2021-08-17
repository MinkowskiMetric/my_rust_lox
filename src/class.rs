use crate::{
    Callable, EnvironmentRef, FuncType, Instance, Interpreter, LoxError, LoxResult,
    ResolvedClassDefinition, ScriptCallable, Value,
};
use std::{fmt, rc::Rc};

#[derive(Clone, Debug)]
pub struct Class {
    class_definition: ResolvedClassDefinition,
    superclass: Option<Rc<Self>>,
    env: EnvironmentRef,
}

impl Class {
    pub fn new(
        class_definition: ResolvedClassDefinition,
        superclass: Option<Rc<Self>>,
        env: &EnvironmentRef,
    ) -> Rc<Self> {
        Rc::new(Self {
            class_definition,
            superclass,
            env: env.clone(),
        })
    }

    pub fn lookup_method(&self, name: &str) -> Option<Rc<ScriptCallable>> {
        self.class_definition
            .methods()
            .get(name)
            .map(|func_definition| ScriptCallable::new(func_definition.clone(), &self.env))
            .or_else(|| {
                self.superclass
                    .as_ref()
                    .and_then(|superclass| superclass.lookup_method(name))
            })
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "class({})", self.class_definition.identifier())
    }
}

impl Callable for Class {
    fn func_type(&self) -> FuncType {
        FuncType::Function
    }

    fn call(
        self: Rc<Self>,
        interpreter: &mut Interpreter,
        arguments: &[Value],
    ) -> LoxResult<Value> {
        let init_method = self.lookup_method("init");
        let instance = Instance::new(self);

        match init_method {
            Some(init_method) => init_method
                .bind(instance.clone())
                .call(interpreter, arguments)
                .map(|_| instance.into()),
            None if arguments.len() == 0 => Ok(instance.into()),
            None => Err(LoxError::IncorrectArgumentCount),
        }
    }

    fn try_into_class(self: Rc<Self>) -> Option<Rc<Class>> {
        Some(self)
    }
}
