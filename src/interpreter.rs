use crate::{
    make_native_function, make_script_function, BinaryOp, CallableReference, Expression,
    ExpressionVisitor, LogicalBinaryOp, LoxError, LoxResult, Nil, Position, Statement,
    StatementVisitor, UnaryOp, UnwindableLoxError, UnwindableLoxResult, Value,
};
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    convert::TryFrom,
    rc::Rc,
};

// The way the book does scope nesting doesn't work, because the borrow checker hates it. This makes sense since
// we're trying to keep a mutable reference to the global object, as well as
struct Enviroment {
    enclosing: Option<Rc<RefCell<Self>>>,
    variables: HashMap<String, Value>,
}

fn add_two_numbers(_interpreter: &mut Interpreter, arguments: &[Value]) -> LoxResult<Value> {
    Ok(Value::from(
        f64::try_from(arguments[0].clone())? + f64::try_from(arguments[1].clone())?,
    ))
}

impl Enviroment {
    fn new_global() -> Self {
        Self {
            enclosing: None,
            variables: HashMap::new(),
        }
    }

    fn new_child(enclosing: Option<Rc<RefCell<Self>>>) -> Self {
        Self {
            enclosing: enclosing,
            variables: HashMap::new(),
        }
    }

    pub fn discard(&mut self) -> Option<Rc<RefCell<Self>>> {
        self.enclosing.take()
    }

    pub fn declare(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.variables.insert(name.to_string(), value);
        Ok(())
    }

    pub fn get(&self, name: &str) -> LoxResult<Value> {
        match self.variables.get(name) {
            Some(v) => Ok(v.clone()),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.borrow().get(name),
                None => Err(LoxError::UnknownVariable(name.to_string())),
            },
        }
    }

    pub fn set(&mut self, name: &str, value: Value) -> LoxResult<()> {
        match self.variables.get_mut(name) {
            Some(v) => {
                *v = value;
                Ok(())
            }
            None => match self.enclosing.as_mut() {
                Some(enclosing) => enclosing.borrow_mut().set(name, value),
                None => Err(LoxError::UnknownVariable(name.to_string())),
            },
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

    pub fn call_function(self, body: &Statement) -> LoxResult<Value> {
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

pub struct Interpreter {
    global_env: Rc<RefCell<Enviroment>>,
    env: Option<Rc<RefCell<Enviroment>>>,
}

impl Interpreter {
    pub fn new() -> LoxResult<Self> {
        let global_env = Enviroment::new_global();

        // The initial environment is the same as the global environment
        let global_env = Rc::new(RefCell::new(global_env));
        let env = Some(global_env.clone());

        let mut ret = Self {
            global_env: global_env,
            env,
        };

        ret.initialize_globals()?;

        Ok(ret)
    }

    fn initialize_globals(&mut self) -> LoxResult<()> {
        self.declare_global("add_two_numbers", make_native_function(2, add_two_numbers))?;

        Ok(())
    }

    pub fn declare_global(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.global_env.borrow_mut().declare(name, value)
    }

    pub fn create_function_frame<'a>(&'a mut self) -> FunctionFrame<'a> {
        self.push_environment();

        FunctionFrame { interpreter: self }
    }

    pub fn accept_statements<'a>(
        &mut self,
        stmts: impl IntoIterator<Item = &'a Statement>,
    ) -> LoxResult<Value> {
        let runner = || -> UnwindableLoxResult<()> {
            for stmt in stmts {
                self.accept_statement(stmt)?;
            }
            Ok(())
        };

        match runner() {
            Ok(_) => Ok(Value::from(Nil)),
            _ => panic!(),
        }
    }

    fn get_env<'a>(&'a self) -> Ref<'a, Enviroment> {
        self.env
            .as_ref()
            .and_then(|r| r.try_borrow().ok())
            .expect("Interpreter environment is inconsistent")
    }

    fn get_env_mut<'a>(&'a mut self) -> RefMut<'a, Enviroment> {
        self.env
            .as_mut()
            .and_then(|r| r.try_borrow_mut().ok())
            .expect("Interpreter environment is inconsistent")
    }

    fn declare_variable(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.get_env_mut().declare(name, value)
    }

    fn get_variable(&self, name: &str) -> LoxResult<Value> {
        self.get_env().get(name)
    }

    fn set_variable(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.get_env_mut().set(name, value)
    }

    fn push_environment(&mut self) {
        let new_environment = Rc::new(RefCell::new(Enviroment::new_child(self.env.take())));
        self.env.replace(new_environment);
    }

    fn pop_environment(&mut self) {
        let outer_environment = self
            .env
            .take()
            .expect("Interpreter environment is inconsistent");

        self.env = outer_environment.borrow_mut().discard();
    }

    fn run_in_nested_environment<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> R {
        self.push_environment();
        let r = f(self);
        self.pop_environment();
        r
    }
}

impl ExpressionVisitor for Interpreter {
    type Return = LoxResult<Value>;

    fn accept_literal(&mut self, _position: &Position, value: &Value) -> Self::Return {
        Ok(value.clone())
    }

    fn accept_unary(
        &mut self,
        _position: &Position,
        op: &UnaryOp,
        expr: &Expression,
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
        left: &Expression,
        op: &BinaryOp,
        right: &Expression,
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
        left: &Expression,
        operator: &LogicalBinaryOp,
        right: &Expression,
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
        comparison: &Expression,
        true_val: &Expression,
        false_val: &Expression,
    ) -> Self::Return {
        if bool::from(self.accept_expression(comparison)?) {
            self.accept_expression(true_val)
        } else {
            self.accept_expression(false_val)
        }
    }

    fn accept_variable_get(&mut self, _position: &Position, name: &str) -> Self::Return {
        self.get_variable(name)
    }

    fn accept_assignment(
        &mut self,
        _position: &Position,
        name: &str,
        value: &Expression,
    ) -> Self::Return {
        let value = self.accept_expression(value)?;
        self.set_variable(name, value.clone()).map(|_| value)
    }

    fn accept_call(
        &mut self,
        _position: &Position,
        callee: &Expression,
        arguments: &[Expression],
    ) -> Self::Return {
        let callee = self.accept_expression(callee)?;
        match CallableReference::try_from(&callee) {
            Ok(callee) => {
                if callee.arity() == arguments.len() {
                    let arguments = arguments
                        .iter()
                        .map(|a| self.accept_expression(a))
                        .collect::<LoxResult<Vec<_>>>()?;
                    callee.call(self, &arguments)
                } else {
                    todo!("This is a runtime error. How do we do those?");
                }
            }
            Err(e) => Err(e),
        }
    }
}

impl StatementVisitor for Interpreter {
    type Return = UnwindableLoxResult<()>;

    fn accept_expression_statement(
        &mut self,
        _position: &Position,
        expr: &Expression,
    ) -> Self::Return {
        self.accept_expression(expr)?;
        Ok(())
    }

    fn accept_print_statement(&mut self, _position: &Position, expr: &Expression) -> Self::Return {
        let output = self.accept_expression(expr)?;
        println!("{}", output);
        Ok(())
    }

    fn accept_var_declaration(
        &mut self,
        _position: &Position,
        identifier: &str,
        expr: &Expression,
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
        body: &Statement,
    ) -> Self::Return {
        let value = make_script_function(parameters, body)?;
        self.declare_variable(identifier, value)?;
        Ok(())
    }

    fn accept_block(&mut self, _position: &Position, statements: &[Statement]) -> Self::Return {
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
        condition: &Expression,
        then_branch: &Statement,
        else_branch: Option<&Statement>,
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
        condition: &Expression,
        body: &Statement,
    ) -> Self::Return {
        loop {
            if !bool::from(self.accept_expression(condition)?) {
                return Ok(());
            } else {
                self.accept_statement(body)?;
            }
        }
    }

    fn accept_return(&mut self, _position: &Position, expr: &Expression) -> Self::Return {
        let value = self.accept_expression(expr)?;

        Err(UnwindableLoxError::Return(value))
    }
}

pub fn interpret<'a>(stmts: impl IntoIterator<Item = &'a Statement>) -> LoxResult<()> {
    Interpreter::new()?.accept_statements(stmts)?;
    Ok(())
}
