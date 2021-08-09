use crate::{
    BinaryOp, Expression, ExpressionVisitor, LoxError, LoxResult, PositionTagged, Statement,
    StatementVisitor, UnaryOp, Value,
};
use std::{collections::HashMap, convert::TryFrom};

struct Enviroment {
    variables: HashMap<String, Value>,
}

impl Enviroment {
    fn new() -> Self {
        Enviroment {
            variables: HashMap::new(),
        }
    }

    fn declare(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.variables.insert(name.to_string(), value);
        Ok(())
    }

    fn get(&self, name: &str) -> LoxResult<Value> {
        match self.variables.get(name) {
            Some(v) => Ok(v.clone()),
            None => Err(LoxError::UnknownVariable(name.to_string())),
        }
    }

    fn set(&mut self, name: &str, value: Value) -> LoxResult<()> {
        match self.variables.get_mut(name) {
            Some(v) => {
                *v = value;
                Ok(())
            }
            None => Err(LoxError::UnknownVariable(name.to_string())),
        }
    }
}

pub struct Interpreter {
    env: Enviroment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Enviroment::new(),
        }
    }

    pub fn accept_statements<'a>(
        &mut self,
        stmts: impl IntoIterator<Item = &'a PositionTagged<Statement>>,
    ) -> LoxResult<()> {
        for stmt in stmts {
            self.accept_statement(stmt.value())?;
        }

        Ok(())
    }

    fn declare_variable(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.env.declare(name, value)
    }

    fn get_variable(&self, name: &str) -> LoxResult<Value> {
        self.env.get(name)
    }

    fn set_variable(&mut self, name: &str, value: Value) -> LoxResult<()> {
        self.env.set(name, value)
    }
}

impl ExpressionVisitor for Interpreter {
    type Return = LoxResult<Value>;

    fn accept_literal(&mut self, value: &Value) -> Self::Return {
        Ok(value.clone())
    }

    fn accept_unary(&mut self, op: &UnaryOp, expr: &Expression) -> Self::Return {
        match op {
            UnaryOp::Bang => {
                let bool_val = bool::try_from(self.accept_expression(expr)?)?;
                Ok((!bool_val).into())
            }

            UnaryOp::Minus => {
                let num_val = f64::try_from(self.accept_expression(expr)?)?;
                Ok((-num_val).into())
            }
        }
    }

    fn accept_binary(
        &mut self,
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

    fn accept_ternary(
        &mut self,
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

    fn accept_variable_get(&mut self, name: &str) -> Self::Return {
        self.get_variable(name)
    }

    fn accept_assignment(&mut self, name: &str, value: &Expression) -> Self::Return {
        let value = self.accept_expression(value)?;
        self.set_variable(name, value.clone()).map(|_| value)
    }
}

impl StatementVisitor for Interpreter {
    type Return = LoxResult<()>;

    fn accept_expression_statement(&mut self, expr: &Expression) -> Self::Return {
        self.accept_expression(expr)?;
        Ok(())
    }

    fn accept_print_statement(&mut self, expr: &Expression) -> Self::Return {
        let output = self.accept_expression(expr)?;
        println!("{}", output);
        Ok(())
    }

    fn accept_var_declaration(&mut self, identifier: &str, expr: &Expression) -> Self::Return {
        let value = self.accept_expression(expr)?;
        self.declare_variable(identifier, value)
    }
}

pub fn interpret<'a>(
    stmts: impl IntoIterator<Item = &'a PositionTagged<Statement>>,
) -> LoxResult<()> {
    Interpreter::new().accept_statements(stmts)
}
