use crate::{Expression, Position};
use std::fmt;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Position, Expression),
    Print(Position, Expression),
    VarDeclaration(Position, String, Expression),
    FuncDeclaration(Position, String, Vec<String>, Box<Statement>),
    Block(Position, Vec<Statement>),
    If(Position, Expression, Box<Statement>, Option<Box<Statement>>),
    While(Position, Expression, Box<Statement>),
    Return(Position, Expression),
}

impl Statement {
    pub fn position(&self) -> &Position {
        match self {
            Self::Expression(pos, ..)
            | Self::Print(pos, ..)
            | Self::VarDeclaration(pos, ..)
            | Self::FuncDeclaration(pos, ..)
            | Self::Block(pos, ..)
            | Self::If(pos, ..)
            | Self::While(pos, ..)
            | Self::Return(pos, ..) => pos,
        }
    }
}

pub trait StatementVisitor {
    type Return;

    fn accept_statement(&mut self, stmt: &Statement) -> Self::Return {
        match stmt {
            Statement::Expression(position, expr) => {
                self.accept_expression_statement(position, expr)
            }
            Statement::Print(position, expr) => self.accept_print_statement(position, expr),
            Statement::VarDeclaration(position, identifier, expr) => {
                self.accept_var_declaration(position, identifier, expr)
            }
            Statement::FuncDeclaration(position, name, parameters, body) => {
                self.accept_func_declaration(position, name, parameters, body)
            }
            Statement::Block(position, statements) => self.accept_block(position, statements),
            Statement::If(position, condition, then_branch, else_branch) => {
                self.accept_if(position, condition, then_branch, else_branch.as_deref())
            }
            Statement::While(position, expression, body) => {
                self.accept_while(position, expression, body)
            }
            Statement::Return(position, expression) => self.accept_return(position, expression),
        }
    }

    fn accept_expression_statement(
        &mut self,
        position: &Position,
        expr: &Expression,
    ) -> Self::Return;
    fn accept_print_statement(&mut self, position: &Position, expr: &Expression) -> Self::Return;
    fn accept_var_declaration(
        &mut self,
        position: &Position,
        identifier: &str,
        expr: &Expression,
    ) -> Self::Return;
    fn accept_func_declaration(
        &mut self,
        position: &Position,
        name: &str,
        parameters: &[String],
        body: &Statement,
    ) -> Self::Return;
    fn accept_block(&mut self, position: &Position, statements: &[Statement]) -> Self::Return;
    fn accept_if(
        &mut self,
        position: &Position,
        condition: &Expression,
        then_branch: &Statement,
        else_branch: Option<&Statement>,
    ) -> Self::Return;
    fn accept_while(
        &mut self,
        position: &Position,
        condition: &Expression,
        body: &Statement,
    ) -> Self::Return;
    fn accept_return(&mut self, position: &Position, expr: &Expression) -> Self::Return;
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

    fn accept_expression_statement(
        &mut self,
        _position: &Position,
        expr: &Expression,
    ) -> Self::Return {
        writeln!(self.f, "{};", expr)
    }

    fn accept_print_statement(&mut self, _position: &Position, expr: &Expression) -> Self::Return {
        writeln!(self.f, "print {};", expr)
    }

    fn accept_var_declaration(
        &mut self,
        _position: &Position,
        identifier: &str,
        expr: &Expression,
    ) -> Self::Return {
        writeln!(self.f, "var {} = {};", identifier, expr)
    }

    fn accept_func_declaration(
        &mut self,
        _position: &Position,
        name: &str,
        parameters: &[String],
        body: &Statement,
    ) -> Self::Return {
        write!(self.f, "fun {}(", name)?;
        for parameter in parameters {
            write!(self.f, "{}, ", parameter)?;
        }
        write!(self.f, ") {}", body)
    }

    fn accept_block(&mut self, _position: &Position, statements: &[Statement]) -> Self::Return {
        // TODOTODOTODO - indentation would be nice
        writeln!(self.f, "{{")?;
        for statement in statements {
            writeln!(self.f, "{}", statement)?;
        }
        writeln!(self.f, "}}")
    }

    fn accept_if(
        &mut self,
        _position: &Position,
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

    fn accept_while(
        &mut self,
        _position: &Position,
        condition: &Expression,
        body: &Statement,
    ) -> Self::Return {
        writeln!(self.f, "while ({}) {}", condition, body)
    }

    fn accept_return(&mut self, _position: &Position, expr: &Expression) -> Self::Return {
        writeln!(self.f, "return {};", expr)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        StatementFormatter::new(f).accept_statement(self)
    }
}
