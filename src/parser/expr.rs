use crate::{Position, Value};
use std::fmt;

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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
pub enum BaseExpression<Identifier: fmt::Display + fmt::Debug + Clone> {
    Literal(Position, Value),
    Unary(Position, UnaryOp, Box<Self>),
    Binary(Position, Box<Self>, BinaryOp, Box<Self>),
    LogicalBinary(Position, Box<Self>, LogicalBinaryOp, Box<Self>),
    Ternary(Position, Box<Self>, Box<Self>, Box<Self>),
    VariableGet(Position, Identifier),
    Assignment(Position, Identifier, Box<Self>),
    Call(Position, Box<Self>, Vec<Self>),
    Get(Position, Box<Self>, String),
    Set(Position, Box<Self>, String, Box<Self>),
    This(Position, Identifier),
    Super(Position, Identifier, Identifier, String),
    ErrorPlaceholder(Position),
}

impl<Identifier: fmt::Display + fmt::Debug + Clone> BaseExpression<Identifier> {
    pub fn position(&self) -> &Position {
        match self {
            Self::Literal(pos, ..)
            | Self::Unary(pos, ..)
            | Self::Binary(pos, ..)
            | Self::LogicalBinary(pos, ..)
            | Self::Ternary(pos, ..)
            | Self::VariableGet(pos, ..)
            | Self::Assignment(pos, ..)
            | Self::Call(pos, ..)
            | Self::Get(pos, ..)
            | Self::Set(pos, ..)
            | Self::This(pos, ..)
            | Self::Super(pos, ..)
            | Self::ErrorPlaceholder(pos, ..) => pos,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedIdentifier {
    Scoped(String, usize),
    Global(String),
}

impl ResolvedIdentifier {
    pub fn scoped_identifier(name: &str, depth: usize) -> Self {
        Self::Scoped(name.to_string(), depth)
    }

    pub fn global_identifier(name: &str) -> Self {
        Self::Global(name.to_string())
    }

    pub fn name(&self) -> &str {
        match self {
            Self::Scoped(name, ..) | Self::Global(name, ..) => name,
        }
    }
}

impl fmt::Display for ResolvedIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(&self.name())
    }
}

pub type Expression = BaseExpression<String>;
pub type ResolvedExpression = BaseExpression<ResolvedIdentifier>;

pub trait ExpressionVisitor<Identifier: fmt::Debug + fmt::Display + Clone> {
    type Return;

    fn accept_expression(&mut self, expr: &BaseExpression<Identifier>) -> Self::Return {
        match expr {
            BaseExpression::<Identifier>::Literal(position, value) => {
                self.accept_literal(position, value)
            }
            BaseExpression::<Identifier>::Unary(position, operator, expr) => {
                self.accept_unary(position, operator, expr)
            }
            BaseExpression::<Identifier>::Binary(position, left, operator, right) => {
                self.accept_binary(position, left, operator, right)
            }
            BaseExpression::<Identifier>::LogicalBinary(position, left, operator, right) => {
                self.accept_logical_binary(position, left, operator, right)
            }
            BaseExpression::<Identifier>::Ternary(position, comparison, true_val, false_val) => {
                self.accept_ternary(position, comparison, true_val, false_val)
            }
            BaseExpression::<Identifier>::VariableGet(position, identifier) => {
                self.accept_variable_get(position, identifier)
            }
            BaseExpression::<Identifier>::Assignment(position, identifier, value) => {
                self.accept_assignment(position, identifier, value)
            }
            BaseExpression::<Identifier>::Call(position, callee, arguments) => {
                self.accept_call(position, callee, arguments)
            }
            BaseExpression::<Identifier>::Get(position, expr, name) => {
                self.accept_get(position, expr, name)
            }
            BaseExpression::<Identifier>::Set(position, expr, name, value) => {
                self.accept_set(position, expr, name, value)
            }
            BaseExpression::<Identifier>::This(position, this_identifier) => {
                self.accept_this(position, this_identifier)
            }
            BaseExpression::<Identifier>::Super(
                position,
                this_identifier,
                super_identifier,
                name,
            ) => self.accept_super(position, this_identifier, super_identifier, name),

            BaseExpression::<Identifier>::ErrorPlaceholder(position) => {
                self.accept_error_expression(position)
            }
        }
    }

    fn accept_literal(&mut self, position: &Position, value: &Value) -> Self::Return;
    fn accept_unary(
        &mut self,
        position: &Position,
        op: &UnaryOp,
        expr: &BaseExpression<Identifier>,
    ) -> Self::Return;
    fn accept_binary(
        &mut self,
        position: &Position,
        left: &BaseExpression<Identifier>,
        op: &BinaryOp,
        right: &BaseExpression<Identifier>,
    ) -> Self::Return;
    fn accept_logical_binary(
        &mut self,
        position: &Position,
        left: &BaseExpression<Identifier>,
        op: &LogicalBinaryOp,
        right: &BaseExpression<Identifier>,
    ) -> Self::Return;
    fn accept_ternary(
        &mut self,
        position: &Position,
        comparison: &BaseExpression<Identifier>,
        true_val: &BaseExpression<Identifier>,
        false_val: &BaseExpression<Identifier>,
    ) -> Self::Return;
    fn accept_variable_get(&mut self, position: &Position, name: &Identifier) -> Self::Return;
    fn accept_assignment(
        &mut self,
        position: &Position,
        name: &Identifier,
        value: &BaseExpression<Identifier>,
    ) -> Self::Return;
    fn accept_call(
        &mut self,
        position: &Position,
        callee: &BaseExpression<Identifier>,
        arguments: &[BaseExpression<Identifier>],
    ) -> Self::Return;
    fn accept_get(
        &mut self,
        position: &Position,
        expr: &BaseExpression<Identifier>,
        name: &String,
    ) -> Self::Return;
    fn accept_set(
        &mut self,
        position: &Position,
        expr: &BaseExpression<Identifier>,
        name: &String,
        value: &BaseExpression<Identifier>,
    ) -> Self::Return;
    fn accept_this(&mut self, position: &Position, this_identifier: &Identifier) -> Self::Return;
    fn accept_super(
        &mut self,
        position: &Position,
        this_identifier: &Identifier,
        super_identifier: &Identifier,
        name: &String,
    ) -> Self::Return;
    fn accept_error_expression(&mut self, position: &Position) -> Self::Return;
}

struct ExpressionPrinter<'a, 'b> {
    f: &'a mut fmt::Formatter<'b>,
}

impl<'a, 'b> ExpressionPrinter<'a, 'b> {
    fn new(f: &'a mut fmt::Formatter<'b>) -> Self {
        Self { f }
    }
}

impl<'a, 'b, Identifier: fmt::Display + fmt::Debug + Clone> ExpressionVisitor<Identifier>
    for ExpressionPrinter<'a, 'b>
{
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
        expression: &BaseExpression<Identifier>,
    ) -> Self::Return {
        write!(self.f, "({}{})", operator, expression)
    }

    fn accept_binary(
        &mut self,
        _position: &Position,
        left: &BaseExpression<Identifier>,
        operator: &BinaryOp,
        right: &BaseExpression<Identifier>,
    ) -> Self::Return {
        write!(self.f, "({} {} {})", left, operator, right)
    }

    fn accept_logical_binary(
        &mut self,
        _position: &Position,
        left: &BaseExpression<Identifier>,
        operator: &LogicalBinaryOp,
        right: &BaseExpression<Identifier>,
    ) -> Self::Return {
        write!(self.f, "({} {} {})", left, operator, right)
    }

    fn accept_ternary(
        &mut self,
        _position: &Position,
        comparison: &BaseExpression<Identifier>,
        true_val: &BaseExpression<Identifier>,
        false_val: &BaseExpression<Identifier>,
    ) -> Self::Return {
        write!(self.f, "({} ? {} : {})", comparison, true_val, false_val)
    }

    fn accept_variable_get(&mut self, _position: &Position, name: &Identifier) -> Self::Return {
        write!(self.f, "{}", name)
    }

    fn accept_assignment(
        &mut self,
        _position: &Position,
        name: &Identifier,
        value: &BaseExpression<Identifier>,
    ) -> Self::Return {
        write!(self.f, "({} = {})", name, value)
    }

    fn accept_call(
        &mut self,
        _position: &Position,
        callee: &BaseExpression<Identifier>,
        arguments: &[BaseExpression<Identifier>],
    ) -> Self::Return {
        write!(self.f, "({})( ", callee)?;

        for argument in arguments {
            write!(self.f, "{},", argument)?;
        }

        write!(self.f, " )")
    }

    fn accept_get(
        &mut self,
        _position: &Position,
        expr: &BaseExpression<Identifier>,
        name: &String,
    ) -> Self::Return {
        write!(self.f, "{}.{}", expr, name)
    }

    fn accept_set(
        &mut self,
        _position: &Position,
        expr: &BaseExpression<Identifier>,
        name: &String,
        value: &BaseExpression<Identifier>,
    ) -> Self::Return {
        write!(self.f, "{}.{} = {}", expr, name, value)
    }

    fn accept_this(&mut self, _position: &Position, _this_identifier: &Identifier) -> Self::Return {
        write!(self.f, "this")
    }

    fn accept_super(
        &mut self,
        _position: &Position,
        _this_identifier: &Identifier,
        _super_identifier: &Identifier,
        name: &String,
    ) -> Self::Return {
        write!(self.f, "super.{}", name)
    }

    fn accept_error_expression(&mut self, position: &Position) -> Self::Return {
        write!(self.f, "ERROR EXPRESSION {}", position)
    }
}

impl<Identifier: fmt::Debug + fmt::Display + Clone> fmt::Display for BaseExpression<Identifier> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        ExpressionPrinter::new(f).accept_expression(self)
    }
}
