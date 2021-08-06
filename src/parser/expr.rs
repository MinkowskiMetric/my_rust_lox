use crate::Token;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Token),
    Unary(Token, Box<Expression>),
    Grouping(Box<Expression>),
    Binary(Box<Expression>, Token, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Literal(token) => token.fmt(f),
            Self::Unary(operator, expr) => write!(f, "{}{}", operator, expr),
            Self::Grouping(expr) => write!(f, "({})", expr),
            Self::Binary(left, operator, right) => write!(f, "{} {} {}", left, operator, right),
        }
    }
}
