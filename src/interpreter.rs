use crate::{
    BinaryOp, Callable, Class, ExpressionVisitor, InstanceRef, LogicalBinaryOp, LoxError,
    LoxResult, NativeCallable, Nil, Position, ResolvedExpression, ResolvedIdentifier,
    ResolvedStatement, ScriptCallable, StatementVisitor, UnaryOp, UnwindableLoxError,
    UnwindableLoxResult, Value,
};
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    convert::{TryFrom, TryInto},
    rc::Rc,
};

// The way the book does scope nesting doesn't work, because the borrow checker hates it. This makes sense since
// we're trying to keep a mutable reference to the global object, as well as
#[derive(Debug)]
pub struct Environment {
    enclosing: Option<EnvironmentRef>,
    variables: HashMap<String, Value>,
}

fn add_two_numbers(_interpreter: &mut Interpreter, arguments: &[Value]) -> LoxResult<Value> {
    Ok(Value::from(
        f64::try_from(arguments[0].clone())? + f64::try_from(arguments[1].clone())?,
    ))
}

impl Environment {
    fn new(enclosing: Option<EnvironmentRef>) -> Self {
        Self {
            enclosing,
            variables: HashMap::new(),
        }
    }

    pub fn declare(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.variables.insert(name.to_string(), value);
        Ok(())
    }

    pub fn get(&self, name: &str, depth: usize) -> LoxResult<Value> {
        if depth == 0 {
            match self.variables.get(name) {
                Some(v) => Ok(v.clone()),
                None => match &self.enclosing {
                    Some(enclosing) => enclosing.borrow().get(name, 0),
                    None => Err(LoxError::UnknownVariable(name.to_string())),
                },
            }
        } else {
            self.enclosing
                .as_ref()
                .expect("Depth is larger than stack")
                .borrow()
                .get(name, depth - 1)
        }
    }

    pub fn set(&mut self, name: &str, depth: usize, value: Value) -> LoxResult<()> {
        if depth == 0 {
            match self.variables.get_mut(name) {
                Some(v) => {
                    *v = value;
                    Ok(())
                }
                None => match self.enclosing.as_mut() {
                    Some(enclosing) => enclosing.borrow_mut().set(name, 0, value),
                    None => Err(LoxError::UnknownVariable(name.to_string())),
                },
            }
        } else {
            self.enclosing
                .as_mut()
                .expect("Depth is larger than stack")
                .borrow_mut()
                .set(name, depth - 1, value)
        }
    }
}

pub struct FunctionFrame<'a> {
    interpreter: &'a mut Interpreter,
}

impl<'a> FunctionFrame<'a> {
    pub fn declare_variable(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.interpreter.declare_variable(name, value)
    }

    pub fn call_function(self, body: &ResolvedStatement) -> LoxResult<Value> {
        match self.interpreter.accept_statement(body) {
            Ok(_) => Ok(Value::from(Nil)),
            Err(UnwindableLoxError::Return(v)) => Ok(v),
            Err(UnwindableLoxError::Error(e)) => Err(e),
        }
    }
}

impl<'a> Drop for FunctionFrame<'a> {
    fn drop(&mut self) {
        self.interpreter.pop_environment()
    }
}

pub type EnvironmentRef = Rc<RefCell<Environment>>;

pub struct Interpreter {
    environment_stack: Vec<EnvironmentRef>,
}

impl Interpreter {
    pub fn new() -> LoxResult<Self> {
        let global_env = Rc::new(RefCell::new(Environment::new(None)));

        let mut ret = Self {
            environment_stack: vec![global_env],
        };

        ret.initialize_globals()?;

        Ok(ret)
    }

    fn initialize_globals(&mut self) -> LoxResult<()> {
        self.declare_global(
            "add_two_numbers",
            NativeCallable::new(add_two_numbers, 2).into(),
        )?;

        Ok(())
    }

    pub fn declare_global(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.get_global_env_mut().declare(name, value)
    }

    pub fn create_function_frame<'a>(&'a mut self, env: &EnvironmentRef) -> FunctionFrame<'a> {
        self.push_function_environment(env.clone());

        FunctionFrame { interpreter: self }
    }

    pub fn accept_statements<'a>(
        &mut self,
        stmts: impl IntoIterator<Item = &'a ResolvedStatement>,
    ) -> LoxResult<Value> {
        let runner = || -> UnwindableLoxResult<()> {
            for stmt in stmts {
                self.accept_statement(stmt)?;
            }
            Ok(())
        };

        match runner() {
            Ok(_) => Ok(Value::from(Nil)),
            c => panic!("Why did we get {:?}", c),
        }
    }

    fn get_env_ref<'a>(&'a self) -> &'a EnvironmentRef {
        self.environment_stack
            .last()
            .expect("Missing global environment")
    }

    fn get_env<'a>(&'a self) -> Ref<'a, Environment> {
        self.get_env_ref().borrow()
    }

    fn get_env_mut<'a>(&'a mut self) -> RefMut<'a, Environment> {
        self.get_env_ref().borrow_mut()
    }

    fn get_global_env<'a>(&'a self) -> Ref<'a, Environment> {
        self.environment_stack[0].borrow()
    }

    fn get_global_env_mut<'a>(&'a self) -> RefMut<'a, Environment> {
        self.environment_stack[0].borrow_mut()
    }

    fn declare_variable(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.get_env_mut().declare(name, value)
    }

    fn get_variable(&self, name: &ResolvedIdentifier) -> LoxResult<Value> {
        let (name, depth, environment) = match name {
            ResolvedIdentifier::Scoped(name, depth) => (name, *depth, self.get_env()),
            ResolvedIdentifier::Global(name) => (name, 0, self.get_global_env()),
        };

        environment.get(name, depth)
    }

    fn set_variable(&mut self, name: &ResolvedIdentifier, value: Value) -> LoxResult<()> {
        let (name, depth, mut environment) = match name {
            ResolvedIdentifier::Scoped(name, depth) => (name, *depth, self.get_env_mut()),
            ResolvedIdentifier::Global(name) => (name, 0, self.get_global_env_mut()),
        };

        environment.set(name, depth, value)
    }

    fn push_nested_environment(&mut self) {
        self.push_function_environment(
            self.environment_stack
                .last()
                .expect("Missing global environment")
                .clone(),
        );
    }

    fn push_function_environment(&mut self, enclosing: EnvironmentRef) {
        let new_environment = Rc::new(RefCell::new(Environment::new(Some(enclosing.clone()))));
        self.environment_stack.push(new_environment);
    }

    fn pop_environment(&mut self) {
        assert!(
            self.environment_stack.len() > 1,
            "Do not pop the global environment"
        );
        self.environment_stack.pop();
    }

    fn run_in_nested_environment<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> R {
        self.push_nested_environment();
        let r = f(self);
        self.pop_environment();
        r
    }
}

impl ExpressionVisitor<ResolvedIdentifier> for Interpreter {
    type Return = LoxResult<Value>;

    fn accept_literal(&mut self, _position: &Position, value: &Value) -> Self::Return {
        Ok(value.clone())
    }

    fn accept_unary(
        &mut self,
        _position: &Position,
        op: &UnaryOp,
        expr: &ResolvedExpression,
    ) -> Self::Return {
        match op {
            UnaryOp::Bang => {
                let bool_val = bool::try_from(self.accept_expression(expr)?)?;
                Ok((!bool_val).into())
            }

            UnaryOp::Minus => {
                let num_val = f64::try_from(self.accept_expression(expr)?)?;
                Ok((-num_val).into())
            }

            UnaryOp::Grouping => self.accept_expression(expr),
        }
    }

    fn accept_binary(
        &mut self,
        _position: &Position,
        left: &ResolvedExpression,
        op: &BinaryOp,
        right: &ResolvedExpression,
    ) -> Self::Return {
        match op {
            BinaryOp::Comma => {
                // Evaluate from left to right, ignore the result of left
                self.accept_expression(left)?;
                self.accept_expression(right)
            }

            BinaryOp::EqualEqual => {
                Ok((self.accept_expression(left)? == self.accept_expression(right)?).into())
            }
            BinaryOp::BangEqual => {
                Ok((self.accept_expression(left)? != self.accept_expression(right)?).into())
            }
            BinaryOp::Greater => Ok((f64::try_from(self.accept_expression(left)?)?
                > f64::try_from(self.accept_expression(right)?)?)
            .into()),
            BinaryOp::GreaterEqual => Ok((f64::try_from(self.accept_expression(left)?)?
                >= f64::try_from(self.accept_expression(right)?)?)
            .into()),
            BinaryOp::Less => Ok((f64::try_from(self.accept_expression(left)?)?
                < f64::try_from(self.accept_expression(right)?)?)
            .into()),
            BinaryOp::LessEqual => Ok((f64::try_from(self.accept_expression(left)?)?
                <= f64::try_from(self.accept_expression(right)?)?)
            .into()),
            BinaryOp::Minus => Ok((f64::try_from(self.accept_expression(left)?)?
                - f64::try_from(self.accept_expression(right)?)?)
            .into()),
            BinaryOp::Star => Ok((f64::try_from(self.accept_expression(left)?)?
                * f64::try_from(self.accept_expression(right)?)?)
            .into()),
            BinaryOp::Slash => Ok((f64::try_from(self.accept_expression(left)?)?
                / f64::try_from(self.accept_expression(right)?)?)
            .into()),

            BinaryOp::Plus => match self.accept_expression(left)? {
                Value::String(s) => {
                    Ok(Value::from(s + &self.accept_expression(right)?.to_string()))
                }
                left => Ok(
                    (f64::try_from(left)? + f64::try_from(self.accept_expression(right)?)?).into(),
                ),
            },
        }
    }

    fn accept_logical_binary(
        &mut self,
        _position: &Position,
        left: &ResolvedExpression,
        operator: &LogicalBinaryOp,
        right: &ResolvedExpression,
    ) -> Self::Return {
        let left = self.accept_expression(left)?;

        match (bool::from(left.clone()), operator) {
            // For an and, if the operand on the left is truthy, evaluate the right
            // For an or, if the operant on the left is falsy, evaluate the right
            (true, LogicalBinaryOp::And) | (false, LogicalBinaryOp::Or) => {
                self.accept_expression(right)
            }

            _ => Ok(left),
        }
    }

    fn accept_ternary(
        &mut self,
        _position: &Position,
        comparison: &ResolvedExpression,
        true_val: &ResolvedExpression,
        false_val: &ResolvedExpression,
    ) -> Self::Return {
        if bool::from(self.accept_expression(comparison)?) {
            self.accept_expression(true_val)
        } else {
            self.accept_expression(false_val)
        }
    }

    fn accept_variable_get(
        &mut self,
        _position: &Position,
        name: &ResolvedIdentifier,
    ) -> Self::Return {
        self.get_variable(name)
    }

    fn accept_assignment(
        &mut self,
        _position: &Position,
        name: &ResolvedIdentifier,
        value: &ResolvedExpression,
    ) -> Self::Return {
        let value = self.accept_expression(value)?;
        self.set_variable(name, value.clone()).map(|_| value)
    }

    fn accept_call(
        &mut self,
        _position: &Position,
        callee: &ResolvedExpression,
        arguments: &[ResolvedExpression],
    ) -> Self::Return {
        let callee = self.accept_expression(callee)?;
        match Rc::<dyn Callable>::try_from(callee) {
            Ok(callee) => {
                if callee.arity() == arguments.len() {
                    let arguments = arguments
                        .iter()
                        .map(|a| self.accept_expression(a))
                        .collect::<LoxResult<Vec<_>>>()?;
                    callee.clone().call(self, &arguments)
                } else {
                    todo!("This is a runtime error. How do we do those?");
                }
            }
            Err(e) => Err(e),
        }
    }

    fn accept_get(
        &mut self,
        _position: &Position,
        object: &ResolvedExpression,
        name: &String,
    ) -> Self::Return {
        let object = self.accept_expression(object)?;
        let object: &InstanceRef = &object.try_into()?;

        object.get(name)
    }

    fn accept_set(
        &mut self,
        _position: &Position,
        object: &ResolvedExpression,
        name: &String,
        value: &ResolvedExpression,
    ) -> Self::Return {
        let object = self.accept_expression(object)?;
        let object: &InstanceRef = &object.try_into()?;

        let value = self.accept_expression(value)?;

        object.set(name, &value)?;
        Ok(value)
    }
}

impl StatementVisitor<ResolvedIdentifier> for Interpreter {
    type Return = UnwindableLoxResult<()>;

    fn accept_expression_statement(
        &mut self,
        _position: &Position,
        expr: &ResolvedExpression,
    ) -> Self::Return {
        self.accept_expression(expr)?;
        Ok(())
    }

    fn accept_print_statement(
        &mut self,
        _position: &Position,
        expr: &ResolvedExpression,
    ) -> Self::Return {
        let output = self.accept_expression(expr)?;
        println!("{}", output);
        Ok(())
    }

    fn accept_var_declaration(
        &mut self,
        _position: &Position,
        identifier: &str,
        expr: &ResolvedExpression,
    ) -> Self::Return {
        let value = self.accept_expression(expr)?;
        self.declare_variable(identifier, value)?;
        Ok(())
    }

    fn accept_func_declaration(
        &mut self,
        _position: &Position,
        identifier: &str,
        parameters: &[String],
        body: &ResolvedStatement,
    ) -> Self::Return {
        let value = ScriptCallable::new(parameters, body, self.get_env_ref()).into();
        self.declare_variable(identifier, value)?;
        Ok(())
    }

    fn accept_class_declaration(
        &mut self,
        _position: &Position,
        name: &str,
        _methods: &[ResolvedStatement],
    ) -> Self::Return {
        let value = Class::new(name).into();
        self.declare_variable(name, value)?;
        Ok(())
    }

    fn accept_block(
        &mut self,
        _position: &Position,
        statements: &[ResolvedStatement],
    ) -> Self::Return {
        self.run_in_nested_environment(|interpreter| {
            for statement in statements {
                interpreter.accept_statement(statement)?;
            }

            Ok(())
        })
    }

    fn accept_if(
        &mut self,
        _position: &Position,
        condition: &ResolvedExpression,
        then_branch: &ResolvedStatement,
        else_branch: Option<&ResolvedStatement>,
    ) -> Self::Return {
        let condition: bool = self.accept_expression(condition)?.into();

        match (condition, else_branch) {
            (true, _) => self.accept_statement(then_branch),
            (false, Some(else_branch)) => self.accept_statement(else_branch),
            (false, None) => Ok(()),
        }
    }

    fn accept_while(
        &mut self,
        _position: &Position,
        condition: &ResolvedExpression,
        body: &ResolvedStatement,
    ) -> Self::Return {
        loop {
            if !bool::from(self.accept_expression(condition)?) {
                return Ok(());
            } else {
                self.accept_statement(body)?;
            }
        }
    }

    fn accept_return(&mut self, _position: &Position, expr: &ResolvedExpression) -> Self::Return {
        let value = self.accept_expression(expr)?;

        Err(UnwindableLoxError::Return(value))
    }
}

pub fn interpret<'a>(stmts: impl IntoIterator<Item = &'a ResolvedStatement>) -> LoxResult<()> {
    Interpreter::new()?.accept_statements(stmts)?;
    Ok(())
}
