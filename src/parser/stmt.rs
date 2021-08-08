use crate::Expression;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
}

pub trait StatementVisitor {
    type Return;

    fn accept_statement(&mut self, stmt: &Statement) -> Self::Return {
        match stmt {
            Statement::Expression(expr) => self.accept_expression_statement(expr),
            Statement::Print(expr) => self.accept_print_statement(expr),
        }
    }

    fn accept_expression_statement(&mut self, expr: &Expression) -> Self::Return;
    fn accept_print_statement(&mut self, expr: &Expression) -> Self::Return;
}

struct StatementFormatter<'a, 'b> {
    f: &'a mut fmt::Formatter<'b>,
}

impl<'a, 'b> StatementFormatter<'a, 'b> {
    fn new(f: &'a mut fmt::Formatter<'b>) -> Self {
        Self { f }
    }
}

impl<'a, 'b> StatementVisitor for StatementFormatter<'a, 'b> {
    type Return = Result<(), fmt::Error>;

    fn accept_expression_statement(&mut self, expr: &Expression) -> Self::Return {
        writeln!(self.f, "{};", expr)
    }

    fn accept_print_statement(&mut self, expr: &Expression) -> Self::Return {
        writeln!(self.f, "print {};", expr)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        StatementFormatter::new(f).accept_statement(self)
    }
}
