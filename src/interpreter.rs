use crate::{Expression, ExpressionVisitor, LoxResult, Token, UnaryOp, Value};
use std::convert::TryFrom;

struct Interpreter;

impl Interpreter {
    fn new() -> Self {
        Self
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

    fn accept_binary(&mut self, left: &Expression, op: &Token, right: &Expression) -> Self::Return {
        todo!()
    }

    fn accept_ternary(
        &mut self,
        comparison: &Expression,
        true_val: &Expression,
        false_val: &Expression,
    ) -> Self::Return {
        todo!()
    }
}

pub fn interpret(expr: &Expression) -> LoxResult<Value> {
    Interpreter::new().accept_expression(expr)
}
