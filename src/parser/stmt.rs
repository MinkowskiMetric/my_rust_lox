use crate::{BaseExpression, Position, ResolvedIdentifier};
use std::{collections::HashMap, fmt};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FuncType {
    Function,
    Method,
    Initializer,
}

impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Function => write!(f, "function"),
            Self::Method => write!(f, "method"),
            Self::Initializer => write!(f, "initializer"),
        }
    }
}

impl FuncType {
    pub fn allows_this(&self) -> bool {
        match self {
            Self::Function => false,
            Self::Method | Self::Initializer => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BaseFuncDefinition<Identifier: fmt::Display + fmt::Debug + Clone> {
    func_type: FuncType,
    identifier: String,
    parameters: Vec<String>,
    body: Box<BaseStatement<Identifier>>,
}

impl<Identifier: fmt::Display + fmt::Debug + Clone> BaseFuncDefinition<Identifier> {
    pub fn new(
        func_type: FuncType,
        identifier: String,
        parameters: Vec<String>,
        body: BaseStatement<Identifier>,
    ) -> Self {
        Self {
            func_type,
            identifier,
            parameters,
            body: Box::new(body),
        }
    }

    pub fn func_type(&self) -> &FuncType {
        &self.func_type
    }
    pub fn identifier(&self) -> &str {
        &self.identifier
    }
    pub fn parameters(&self) -> &[String] {
        &self.parameters
    }
    pub fn body(&self) -> &BaseStatement<Identifier> {
        &self.body
    }
}

pub type FuncDefinition = BaseFuncDefinition<String>;
pub type ResolvedFuncDefinition = BaseFuncDefinition<ResolvedIdentifier>;

#[derive(Debug, Clone)]
pub struct BaseClassDefinition<Identifier: fmt::Display + fmt::Debug + Clone> {
    identifier: String,
    superclass_identifier: Option<Identifier>,
    methods: HashMap<String, BaseFuncDefinition<Identifier>>,
}

pub type ClassDefinition = BaseClassDefinition<String>;
pub type ResolvedClassDefinition = BaseClassDefinition<ResolvedIdentifier>;

impl<Identifier: fmt::Display + fmt::Debug + Clone> BaseClassDefinition<Identifier> {
    pub fn new(
        identifier: String,
        superclass_identifier: Option<Identifier>,
        methods: HashMap<String, BaseFuncDefinition<Identifier>>,
    ) -> Self {
        Self {
            identifier,
            superclass_identifier,
            methods,
        }
    }

    pub fn identifier(&self) -> &str {
        &self.identifier
    }
    pub fn superclass_identifier(&self) -> Option<&Identifier> {
        self.superclass_identifier.as_ref()
    }
    pub fn methods(&self) -> &HashMap<String, BaseFuncDefinition<Identifier>> {
        &self.methods
    }
}

#[derive(Debug, Clone)]
pub enum BaseStatement<Identifier: fmt::Display + fmt::Debug + Clone> {
    Expression(Position, BaseExpression<Identifier>),
    Print(Position, BaseExpression<Identifier>),
    VarDeclaration(Position, String, BaseExpression<Identifier>),
    FuncDeclaration(Position, BaseFuncDefinition<Identifier>),
    ClassDeclaration(Position, BaseClassDefinition<Identifier>),
    Block(Position, Vec<Self>),
    If(
        Position,
        BaseExpression<Identifier>,
        Box<Self>,
        Option<Box<Self>>,
    ),
    While(Position, BaseExpression<Identifier>, Box<Self>),
    Return(Position, Option<BaseExpression<Identifier>>),
    ErrorPlaceholder(Position),
}

impl<Identifier: fmt::Display + fmt::Debug + Clone> BaseStatement<Identifier> {
    pub fn position(&self) -> &Position {
        match self {
            Self::Expression(pos, ..)
            | Self::Print(pos, ..)
            | Self::VarDeclaration(pos, ..)
            | Self::FuncDeclaration(pos, ..)
            | Self::ClassDeclaration(pos, ..)
            | Self::Block(pos, ..)
            | Self::If(pos, ..)
            | Self::While(pos, ..)
            | Self::Return(pos, ..)
            | Self::ErrorPlaceholder(pos, ..) => pos,
        }
    }
}

pub type Statement = BaseStatement<String>;
pub type ResolvedStatement = BaseStatement<ResolvedIdentifier>;

pub trait StatementVisitor<Identifier: fmt::Display + fmt::Debug + Clone> {
    type Return;

    fn accept_statement(&mut self, stmt: &BaseStatement<Identifier>) -> Self::Return {
        match stmt {
            BaseStatement::<Identifier>::Expression(position, expr) => {
                self.accept_expression_statement(position, expr)
            }
            BaseStatement::<Identifier>::Print(position, expr) => {
                self.accept_print_statement(position, expr)
            }
            BaseStatement::<Identifier>::VarDeclaration(position, identifier, expr) => {
                self.accept_var_declaration(position, identifier, expr)
            }
            BaseStatement::<Identifier>::FuncDeclaration(position, func_definition) => {
                self.accept_func_declaration(position, func_definition)
            }
            BaseStatement::<Identifier>::ClassDeclaration(position, class_definition) => {
                self.accept_class_declaration(position, class_definition)
            }
            BaseStatement::<Identifier>::Block(position, statements) => {
                self.accept_block(position, statements)
            }
            BaseStatement::<Identifier>::If(position, condition, then_branch, else_branch) => {
                self.accept_if(position, condition, then_branch, else_branch.as_deref())
            }
            BaseStatement::<Identifier>::While(position, expression, body) => {
                self.accept_while(position, expression, body)
            }
            BaseStatement::<Identifier>::Return(position, expression) => {
                self.accept_return(position, expression.as_ref())
            }
            BaseStatement::<Identifier>::ErrorPlaceholder(position) => {
                self.accept_error_statement(position)
            }
        }
    }

    fn accept_expression_statement(
        &mut self,
        position: &Position,
        expr: &BaseExpression<Identifier>,
    ) -> Self::Return;
    fn accept_print_statement(
        &mut self,
        position: &Position,
        expr: &BaseExpression<Identifier>,
    ) -> Self::Return;
    fn accept_var_declaration(
        &mut self,
        position: &Position,
        identifier: &str,
        expr: &BaseExpression<Identifier>,
    ) -> Self::Return;
    fn accept_func_declaration(
        &mut self,
        position: &Position,
        func_definition: &BaseFuncDefinition<Identifier>,
    ) -> Self::Return;
    fn accept_class_declaration(
        &mut self,
        position: &Position,
        class_definition: &BaseClassDefinition<Identifier>,
    ) -> Self::Return;
    fn accept_block(
        &mut self,
        position: &Position,
        statements: &[BaseStatement<Identifier>],
    ) -> Self::Return;
    fn accept_if(
        &mut self,
        position: &Position,
        condition: &BaseExpression<Identifier>,
        then_branch: &BaseStatement<Identifier>,
        else_branch: Option<&BaseStatement<Identifier>>,
    ) -> Self::Return;
    fn accept_while(
        &mut self,
        position: &Position,
        condition: &BaseExpression<Identifier>,
        body: &BaseStatement<Identifier>,
    ) -> Self::Return;
    fn accept_return(
        &mut self,
        position: &Position,
        expr: Option<&BaseExpression<Identifier>>,
    ) -> Self::Return;
    fn accept_error_statement(&mut self, position: &Position) -> Self::Return;
}

struct StatementFormatter<'a, 'b> {
    f: &'a mut fmt::Formatter<'b>,
}

impl<'a, 'b> StatementFormatter<'a, 'b> {
    fn new(f: &'a mut fmt::Formatter<'b>) -> Self {
        Self { f }
    }
}

impl<'a, 'b, Identifier: fmt::Display + fmt::Debug + Clone> StatementVisitor<Identifier>
    for StatementFormatter<'a, 'b>
{
    type Return = Result<(), fmt::Error>;

    fn accept_expression_statement(
        &mut self,
        _position: &Position,
        expr: &BaseExpression<Identifier>,
    ) -> Self::Return {
        writeln!(self.f, "{};", expr)
    }

    fn accept_print_statement(
        &mut self,
        _position: &Position,
        expr: &BaseExpression<Identifier>,
    ) -> Self::Return {
        writeln!(self.f, "print {};", expr)
    }

    fn accept_var_declaration(
        &mut self,
        _position: &Position,
        identifier: &str,
        expr: &BaseExpression<Identifier>,
    ) -> Self::Return {
        writeln!(self.f, "var {} = {};", identifier, expr)
    }

    fn accept_func_declaration(
        &mut self,
        _position: &Position,
        func_definition: &BaseFuncDefinition<Identifier>,
    ) -> Self::Return {
        match func_definition.func_type() {
            FuncType::Function => write!(self.f, "fun {} (", func_definition.identifier())?,
            FuncType::Method | FuncType::Initializer => {
                write!(self.f, "{} (", func_definition.identifier())?
            }
        };

        for parameter in func_definition.parameters() {
            write!(self.f, "{}, ", parameter)?;
        }
        write!(self.f, ") {}", func_definition.body())
    }

    fn accept_class_declaration(
        &mut self,
        _position: &Position,
        class_definition: &BaseClassDefinition<Identifier>,
    ) -> Self::Return {
        match class_definition.superclass_identifier() {
            Some(superclass_identifier) => writeln!(
                self.f,
                "class {} < {} {{",
                class_definition.identifier(),
                superclass_identifier
            ),
            None => writeln!(self.f, "class {} {{", class_definition.identifier()),
        }?;

        for (_, method) in class_definition.methods() {
            write!(self.f, "{} (", method.identifier())?;
            for parameter in method.parameters() {
                write!(self.f, "{}, ", parameter)?;
            }
            write!(self.f, ") {}", method.body())?
        }
        writeln!(self.f, "}};")
    }

    fn accept_block(
        &mut self,
        _position: &Position,
        statements: &[BaseStatement<Identifier>],
    ) -> Self::Return {
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
        condition: &BaseExpression<Identifier>,
        then_branch: &BaseStatement<Identifier>,
        else_branch: Option<&BaseStatement<Identifier>>,
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
        condition: &BaseExpression<Identifier>,
        body: &BaseStatement<Identifier>,
    ) -> Self::Return {
        writeln!(self.f, "while ({}) {}", condition, body)
    }

    fn accept_return(
        &mut self,
        _position: &Position,
        expr: Option<&BaseExpression<Identifier>>,
    ) -> Self::Return {
        match expr {
            Some(expr) => writeln!(self.f, "return {};", expr),
            None => writeln!(self.f, "return;"),
        }
    }

    fn accept_error_statement(&mut self, position: &Position) -> Self::Return {
        writeln!(self.f, "ERROR STATEMENT: {}", position)
    }
}

impl<Identifier: fmt::Display + fmt::Debug + Clone> fmt::Display for BaseStatement<Identifier> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        StatementFormatter::new(f).accept_statement(self)
    }
}
