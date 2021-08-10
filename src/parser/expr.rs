use crate::{Position, Value};
use std::fmt;

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Minus,
    Bang,
    Grouping,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Bang => write!(f, "!"),
            UnaryOp::Grouping => write!(f, "<GROUP>"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Comma,
    EqualEqual,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Plus,
    Minus,
    Star,
    Slash,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            BinaryOp::Comma => write!(f, ","),
            BinaryOp::EqualEqual => write!(f, "=="),
            BinaryOp::BangEqual => write!(f, "!="),
            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::GreaterEqual => write!(f, ">="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::LessEqual => write!(f, "<="),
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Star => write!(f, "*"),
            BinaryOp::Slash => write!(f, "/"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LogicalBinaryOp {
    And,
    Or,
}

impl fmt::Display for LogicalBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(match self {
            Self::And => "and",
            Self::Or => "or",
        })
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Position, Value),
    Unary(Position, UnaryOp, Box<Expression>),
    Binary(Position, Box<Expression>, BinaryOp, Box<Expression>),
    LogicalBinary(Position, Box<Expression>, LogicalBinaryOp, Box<Expression>),
    Ternary(Position, Box<Expression>, Box<Expression>, Box<Expression>),
    VariableGet(Position, String),
    Assignment(Position, String, Box<Expression>),
    Call(Position, Box<Expression>, Vec<Expression>),
}

impl Expression {
    pub fn position(&self) -> &Position {
        match self {
            Self::Literal(pos, ..)
            | Self::Unary(pos, ..)
            | Self::Binary(pos, ..)
            | Self::LogicalBinary(pos, ..)
            | Self::Ternary(pos, ..)
            | Self::VariableGet(pos, ..)
            | Self::Assignment(pos, ..)
            | Self::Call(pos, ..) => pos,
        }
    }
}
pub trait ExpressionVisitor {
    type Return;

    fn accept_expression(&mut self, expr: &Expression) -> Self::Return {
        match expr {
            Expression::Literal(position, value) => self.accept_literal(position, value),
            Expression::Unary(position, operator, expr) => {
                self.accept_unary(position, operator, expr)
            }
            Expression::Binary(position, left, operator, right) => {
                self.accept_binary(position, left, operator, right)
            }
            Expression::LogicalBinary(position, left, operator, right) => {
                self.accept_logical_binary(position, left, operator, right)
            }
            Expression::Ternary(position, comparison, true_val, false_val) => {
                self.accept_ternary(position, comparison, true_val, false_val)
            }
            Expression::VariableGet(position, identifier) => {
                self.accept_variable_get(position, identifier)
            }
            Expression::Assignment(position, identifier, value) => {
                self.accept_assignment(position, identifier, value)
            }
            Expression::Call(position, callee, arguments) => {
                self.accept_call(position, callee, arguments)
            }
        }
    }

    fn accept_literal(&mut self, position: &Position, value: &Value) -> Self::Return;
    fn accept_unary(
        &mut self,
        position: &Position,
        op: &UnaryOp,
        expr: &Expression,
    ) -> Self::Return;
    fn accept_binary(
        &mut self,
        position: &Position,
        left: &Expression,
        op: &BinaryOp,
        right: &Expression,
    ) -> Self::Return;
    fn accept_logical_binary(
        &mut self,
        position: &Position,
        left: &Expression,
        op: &LogicalBinaryOp,
        right: &Expression,
    ) -> Self::Return;
    fn accept_ternary(
        &mut self,
        position: &Position,
        comparison: &Expression,
        true_val: &Expression,
        false_val: &Expression,
    ) -> Self::Return;
    fn accept_variable_get(&mut self, position: &Position, name: &str) -> Self::Return;
    fn accept_assignment(
        &mut self,
        position: &Position,
        name: &str,
        value: &Expression,
    ) -> Self::Return;
    fn accept_call(
        &mut self,
        position: &Position,
        callee: &Expression,
        arguments: &[Expression],
    ) -> Self::Return;
}

struct ExpressionPrinter<'a, 'b> {
    f: &'a mut fmt::Formatter<'b>,
}

impl<'a, 'b> ExpressionPrinter<'a, 'b> {
    fn new(f: &'a mut fmt::Formatter<'b>) -> Self {
        Self { f }
    }
}

impl<'a, 'b> ExpressionVisitor for ExpressionPrinter<'a, 'b> {
    type Return = Result<(), fmt::Error>;

    fn accept_literal(&mut self, _position: &Position, value: &Value) -> Self::Return {
        match value {
            Value::String(s) => write!(self.f, "\"{}\"", s),
            value => write!(self.f, "{}", value),
        }
    }

    fn accept_unary(
        &mut self,
        _position: &Position,
        operator: &UnaryOp,
        expression: &Expression,
    ) -> Self::Return {
        write!(self.f, "({}{})", operator, expression)
    }

    fn accept_binary(
        &mut self,
        _position: &Position,
        left: &Expression,
        operator: &BinaryOp,
        right: &Expression,
    ) -> Self::Return {
        write!(self.f, "({} {} {})", left, operator, right)
    }

    fn accept_logical_binary(
        &mut self,
        _position: &Position,
        left: &Expression,
        operator: &LogicalBinaryOp,
        right: &Expression,
    ) -> Self::Return {
        write!(self.f, "({} {} {})", left, operator, right)
    }

    fn accept_ternary(
        &mut self,
        _position: &Position,
        comparison: &Expression,
        true_val: &Expression,
        false_val: &Expression,
    ) -> Self::Return {
        write!(self.f, "({} ? {} : {})", comparison, true_val, false_val)
    }

    fn accept_variable_get(&mut self, _position: &Position, name: &str) -> Self::Return {
        self.f.write_str(name)
    }

    fn accept_assignment(
        &mut self,
        _position: &Position,
        name: &str,
        value: &Expression,
    ) -> Self::Return {
        write!(self.f, "({} = {})", name, value)
    }

    fn accept_call(
        &mut self,
        _position: &Position,
        callee: &Expression,
        arguments: &[Expression],
    ) -> Self::Return {
        write!(self.f, "({})( ", callee)?;

        for argument in arguments {
            write!(self.f, "{},", argument)?;
        }

        write!(self.f, " )")
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        ExpressionPrinter::new(f).accept_expression(self)
    }
}
