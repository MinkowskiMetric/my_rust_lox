use crate::Expression;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    VarDeclaration(String, Expression),
    Block(Vec<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
}

pub trait StatementVisitor {
    type Return;

    fn accept_statement(&mut self, stmt: &Statement) -> Self::Return {
        match stmt {
            Statement::Expression(expr) => self.accept_expression_statement(expr),
            Statement::Print(expr) => self.accept_print_statement(expr),
            Statement::VarDeclaration(identifier, expr) => {
                self.accept_var_declaration(identifier, expr)
            }
            Statement::Block(statements) => self.accept_block(statements),
            Statement::If(condition, then_branch, else_branch) => {
                self.accept_if(condition, then_branch, else_branch.as_deref())
            }
        }
    }

    fn accept_expression_statement(&mut self, expr: &Expression) -> Self::Return;
    fn accept_print_statement(&mut self, expr: &Expression) -> Self::Return;
    fn accept_var_declaration(&mut self, identifier: &str, expr: &Expression) -> Self::Return;
    fn accept_block(&mut self, statements: &[Statement]) -> Self::Return;
    fn accept_if(
        &mut self,
        condition: &Expression,
        then_branch: &Statement,
        else_branch: Option<&Statement>,
    ) -> Self::Return;
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

    fn accept_var_declaration(&mut self, identifier: &str, expr: &Expression) -> Self::Return {
        writeln!(self.f, "var {} = {};", identifier, expr)
    }

    fn accept_block(&mut self, statements: &[Statement]) -> Self::Return {
        // TODOTODOTODO - indentation would be nice
        writeln!(self.f, "{{")?;
        for statement in statements {
            writeln!(self.f, "{}", statement)?;
        }
        writeln!(self.f, "}}")
    }

    fn accept_if(
        &mut self,
        condition: &Expression,
        then_branch: &Statement,
        else_branch: Option<&Statement>,
    ) -> Self::Return {
        match else_branch {
            Some(else_branch) => writeln!(
                self.f,
                "if ({}) {} else {}",
                condition, then_branch, else_branch
            ),
            None => writeln!(self.f, "if ({}) {}", condition, then_branch),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        StatementFormatter::new(f).accept_statement(self)
    }
}
