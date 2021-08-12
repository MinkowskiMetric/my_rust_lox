use crate::{
    Callable, EnvironmentRef, FuncType, Instance, Interpreter, LoxResult, ResolvedStatement,
    ScriptCallable, Value,
};
use std::{collections::HashMap, convert::TryInto, fmt, rc::Rc};

#[derive(Clone, Debug)]
pub struct Class {
    name: String,
    methods: HashMap<String, ResolvedStatement>,
    env: EnvironmentRef,
}

impl Class {
    pub fn new(
        name: &str,
        methods: HashMap<String, ResolvedStatement>,
        env: &EnvironmentRef,
    ) -> Rc<Self> {
        Rc::new(Self {
            name: name.to_string(),
            methods,
            env: env.clone(),
        })
    }

    pub fn lookup_method(&self, name: &str) -> Option<Rc<ScriptCallable>> {
        self.methods.get(name).map(|stmt| match stmt {
            ResolvedStatement::FuncDeclaration(_, func_type, _, parameters, body) => {
                ScriptCallable::new(*func_type, parameters, body, &self.env)
            }
            stmt => panic!("Unexpected statement {:?} in class method {}", stmt, name),
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
    fn arity(&self) -> usize {
        0
    }

    fn call(
        self: Rc<Self>,
        interpreter: &mut Interpreter,
        arguments: &[Value],
    ) -> LoxResult<Value> {
        let instance = Instance::new(self);
        match instance.clone().get("init") {
            Ok(initializer) => {
                let initializer: Rc<dyn Callable> = initializer.try_into()?;
                initializer.call(interpreter, arguments)?;
            }

            Err(_) => (),
        };

        Ok(instance.into())
    }
}
