use std::fmt;

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(crate::Token),
    Unary(crate::Token, Box<Expression>),
    Grouping(Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Literal(token) => token.fmt(f),
            Self::Unary(operator, expr) => write!(f, "{}{}", operator, expr),
            Self::Grouping(expr) => write!(f, "({})", expr),
        }
    }
}
