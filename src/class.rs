use crate::{
    Callable, EnvironmentRef, FuncType, Instance, Interpreter, LoxError, LoxResult,
    ResolvedStatement, ScriptCallable, Value,
};
use std::{collections::HashMap, fmt, rc::Rc};

#[derive(Clone, Debug)]
pub struct Class {
    name: String,
    superclass: Option<Rc<Self>>,
    methods: HashMap<String, ResolvedStatement>,
    env: EnvironmentRef,
}

impl Class {
    pub fn new(
        name: &str,
        superclass: Option<Rc<Self>>,
        methods: HashMap<String, ResolvedStatement>,
        env: &EnvironmentRef,
    ) -> Rc<Self> {
        Rc::new(Self {
            name: name.to_string(),
            superclass,
            methods,
            env: env.clone(),
        })
    }

    pub fn lookup_method(&self, name: &str) -> Option<Rc<ScriptCallable>> {
        self.methods
            .get(name)
            .map(|stmt| match stmt {
                ResolvedStatement::FuncDeclaration(_, func_type, _, parameters, body) => {
                    ScriptCallable::new(*func_type, parameters, body, &self.env)
                }
                stmt => panic!("Unexpected statement {:?} in class method {}", stmt, name),
            })
            .or_else(|| {
                self.superclass
                    .as_ref()
                    .and_then(|superclass| superclass.lookup_method(name))
            })
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
