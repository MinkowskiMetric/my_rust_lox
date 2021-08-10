use crate::{
    BinaryOp, Expression, ExpressionVisitor, LogicalBinaryOp, LoxError, LoxResult, Position,
    Statement, StatementVisitor, UnaryOp, Value,
};
use std::{collections::HashMap, convert::TryFrom};

struct Enviroment {
    enclosing: Option<Box<Self>>,
    variables: HashMap<String, Value>,
}

impl Enviroment {
    fn new(enclosing: Option<Box<Self>>) -> Self {
        Self {
            enclosing,
            variables: HashMap::new(),
        }
    }

    pub fn discard(mut self) -> Option<Box<Self>> {
        self.enclosing.take()
    }

    pub fn declare(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.variables.insert(name.to_string(), value);
        Ok(())
    }

    pub fn get(&self, name: &str) -> LoxResult<&Value> {
        match self.variables.get(name) {
            Some(v) => Ok(v),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.get(name),
                None => Err(LoxError::UnknownVariable(name.to_string())),
            },
        }
    }

    pub fn get_mut(&mut self, name: &str) -> LoxResult<&mut Value> {
        match self.variables.get_mut(name) {
            Some(v) => Ok(v),
            None => match self.enclosing.as_mut() {
                Some(enclosing) => enclosing.get_mut(name),
                None => Err(LoxError::UnknownVariable(name.to_string())),
            },
        }
    }
}

pub struct Interpreter {
    env: Option<Box<Enviroment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Some(Box::new(Enviroment::new(None))),
        }
    }

    pub fn accept_statements<'a>(
        &mut self,
        stmts: impl IntoIterator<Item = &'a Statement>,
    ) -> LoxResult<()> {
        for stmt in stmts {
            self.accept_statement(stmt)?;
        }

        Ok(())
    }

    fn get_env(&self) -> &Enviroment {
        self.env
            .as_ref()
            .expect("Interpreter environment is inconsistent")
    }

    fn get_env_mut(&mut self) -> &mut Enviroment {
        self.env
            .as_mut()
            .expect("Interpreter environment is inconsistent")
    }

    fn declare_variable(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.get_env_mut().declare(name, value)
    }

    fn get_variable(&self, name: &str) -> LoxResult<Value> {
        Ok(self.get_env().get(name)?.clone())
    }

    fn set_variable(&mut self, name: &str, value: Value) -> LoxResult<()> {
        *self.get_env_mut().get_mut(name)? = value;
        Ok(())
    }

    fn push_environment(&mut self) {
        let new_environment = Box::new(Enviroment::new(self.env.take()));
        self.env.replace(new_environment);
    }

    fn pop_environment(&mut self) {
        let outer_environment = self
            .env
            .take()
            .expect("Interpreter environment is inconsistent")
            .discard();
        self.env = outer_environment;
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
        todo!("CALL {} with {:?}", callee, arguments)
    }
}

impl StatementVisitor for Interpreter {
    type Return = LoxResult<()>;

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
        self.declare_variable(identifier, value)
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
}

pub fn interpret<'a>(stmts: impl IntoIterator<Item = &'a Statement>) -> LoxResult<()> {
    Interpreter::new().accept_statements(stmts)
}
