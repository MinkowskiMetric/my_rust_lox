use crate::Value;
use std::fmt;

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Minus,
    Bang,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Bang => write!(f, "!"),
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
    Literal(Value),
    Unary(UnaryOp, Box<Expression>),
    Grouping(Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    LogicalBinary(Box<Expression>, LogicalBinaryOp, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    VariableGet(String),
    Assignment(String, Box<Expression>),
}

pub trait ExpressionVisitor {
    type Return;

    fn accept_expression(&mut self, expr: &Expression) -> Self::Return {
        match expr {
            Expression::Literal(value) => self.accept_literal(value),
            Expression::Unary(operator, expr) => self.accept_unary(operator, expr),
            Expression::Grouping(expr) => self.accept_grouping(expr),
            Expression::Binary(left, operator, right) => self.accept_binary(left, operator, right),
            Expression::LogicalBinary(left, operator, right) => {
                self.accept_logical_binary(left, operator, right)
            }
            Expression::Ternary(comparison, true_val, false_val) => {
                self.accept_ternary(comparison, true_val, false_val)
            }
            Expression::VariableGet(identifier) => self.accept_variable_get(identifier),
            Expression::Assignment(identifier, value) => self.accept_assignment(identifier, value),
        }
    }

    fn accept_literal(&mut self, value: &Value) -> Self::Return;
    fn accept_unary(&mut self, op: &UnaryOp, expr: &Expression) -> Self::Return;
    fn accept_binary(
        &mut self,
        left: &Expression,
        op: &BinaryOp,
        right: &Expression,
    ) -> Self::Return;
    fn accept_logical_binary(
        &mut self,
        left: &Expression,
        op: &LogicalBinaryOp,
        right: &Expression,
    ) -> Self::Return;
    fn accept_ternary(
        &mut self,
        comparison: &Expression,
        true_val: &Expression,
        false_val: &Expression,
    ) -> Self::Return;
    fn accept_variable_get(&mut self, name: &str) -> Self::Return;
    fn accept_assignment(&mut self, name: &str, value: &Expression) -> Self::Return;

    // This might not be the best idea, but most of the time you don't care about these
    fn accept_grouping(&mut self, expr: &Expression) -> Self::Return {
        self.accept_expression(expr)
    }
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

    fn accept_literal(&mut self, value: &Value) -> Self::Return {
        match value {
            Value::String(s) => write!(self.f, "\"{}\"", s),
            value => write!(self.f, "{}", value),
        }
    }

    fn accept_unary(&mut self, operator: &UnaryOp, expression: &Expression) -> Self::Return {
        write!(self.f, "({}{})", operator, expression)
    }

    fn accept_binary(
        &mut self,
        left: &Expression,
        operator: &BinaryOp,
        right: &Expression,
    ) -> Self::Return {
        write!(self.f, "({} {} {})", left, operator, right)
    }

    fn accept_logical_binary(
        &mut self,
        left: &Expression,
        operator: &LogicalBinaryOp,
        right: &Expression,
    ) -> Self::Return {
        write!(self.f, "({} {} {})", left, operator, right)
    }

    fn accept_ternary(
        &mut self,
        comparison: &Expression,
        true_val: &Expression,
        false_val: &Expression,
    ) -> Self::Return {
        write!(self.f, "({} ? {} : {})", comparison, true_val, false_val)
    }

    fn accept_grouping(&mut self, expr: &Expression) -> Self::Return {
        write!(self.f, "({})", expr)
    }

    fn accept_variable_get(&mut self, name: &str) -> Self::Return {
        self.f.write_str(name)
    }

    fn accept_assignment(&mut self, name: &str, value: &Expression) -> Self::Return {
        write!(self.f, "({} = {})", name, value)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        ExpressionPrinter::new(f).accept_expression(self)
    }
}
