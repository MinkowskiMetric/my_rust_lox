use crate::{
    BinaryOp, Expression, ExpressionVisitor, FuncType, LogicalBinaryOp, LoxError, LoxResult,
    Position, ResolvedExpression, ResolvedIdentifier, ResolvedStatement, Statement,
    StatementVisitor, UnaryOp, Value,
};
use std::collections::HashMap;

#[derive(Clone, Copy, Debug)]
enum ClassType {
    Class,
    None,
}

struct Resolver {
    scopes: Vec<HashMap<String, (Position, bool)>>,
    class_type: ClassType,
}

impl Resolver {
    fn new() -> Self {
        Self {
            scopes: Vec::new(),
            class_type: ClassType::None,
        }
    }
}

impl Resolver {
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn check_duplicate(&self, duplicate_pos: &Position, name: &str) -> LoxResult<()> {
        match self.scopes.iter().rev().filter_map(|m| m.get(name)).next() {
            Some((original_pos, _)) => Err(LoxError::DuplicateVariable(
                name.to_string(),
                original_pos.clone(),
                duplicate_pos.clone(),
            )),
            None => Ok(()),
        }
    }

    fn declare(&mut self, pos: &Position, name: &str) -> LoxResult<()> {
        self.check_duplicate(pos, name)?;

        Ok(match self.scopes.last_mut() {
            Some(scope) => {
                scope.insert(name.to_string(), (pos.clone(), false));
            }
            None => (),
        })
    }

    fn define(&mut self, name: &str) {
        match self.scopes.last_mut() {
            Some(scope) => {
                scope.get_mut(name).expect("Should declare before define").1 = true;
            }

            None => (),
        }
    }

    fn resolve_local(&mut self, name: &str) -> ResolvedIdentifier {
        (0..self.scopes.len())
            .rev()
            .find(|depth| self.scopes[*depth].contains_key(name))
            .map(|depth| ResolvedIdentifier::scoped_identifier(name, self.scopes.len() - 1 - depth))
            .unwrap_or_else(|| ResolvedIdentifier::global_identifier(name))
    }

    fn resolve_this(&mut self, pos: &Position) -> LoxResult<ResolvedIdentifier> {
        match self.class_type {
            ClassType::Class => Ok(self.resolve_local("this")),
            ClassType::None => Err(LoxError::ThisOutsideMethod(pos.clone())),
        }
    }
}

impl ExpressionVisitor<String> for Resolver {
    type Return = LoxResult<ResolvedExpression>;

    fn accept_literal(&mut self, position: &Position, value: &Value) -> Self::Return {
        Ok(ResolvedExpression::Literal(position.clone(), value.clone()))
    }

    fn accept_unary(
        &mut self,
        position: &Position,
        op: &UnaryOp,
        expr: &Expression,
    ) -> Self::Return {
        let expr = self.accept_expression(expr)?;

        Ok(ResolvedExpression::Unary(
            position.clone(),
            *op,
            Box::new(expr),
        ))
    }

    fn accept_binary(
        &mut self,
        position: &Position,
        left: &Expression,
        op: &BinaryOp,
        right: &Expression,
    ) -> Self::Return {
        let left = self.accept_expression(left)?;
        let right = self.accept_expression(right)?;

        Ok(ResolvedExpression::Binary(
            position.clone(),
            Box::new(left),
            *op,
            Box::new(right),
        ))
    }

    fn accept_logical_binary(
        &mut self,
        position: &Position,
        left: &Expression,
        op: &LogicalBinaryOp,
        right: &Expression,
    ) -> Self::Return {
        let left = self.accept_expression(left)?;
        let right = self.accept_expression(right)?;

        Ok(ResolvedExpression::LogicalBinary(
            position.clone(),
            Box::new(left),
            *op,
            Box::new(right),
        ))
    }

    fn accept_ternary(
        &mut self,
        position: &Position,
        comparison: &Expression,
        true_val: &Expression,
        false_val: &Expression,
    ) -> Self::Return {
        let comparison = self.accept_expression(comparison)?;
        let true_val = self.accept_expression(true_val)?;
        let false_val = self.accept_expression(false_val)?;

        Ok(ResolvedExpression::Ternary(
            position.clone(),
            Box::new(comparison),
            Box::new(true_val),
            Box::new(false_val),
        ))
    }

    fn accept_variable_get(&mut self, position: &Position, name: &String) -> Self::Return {
        Ok(ResolvedExpression::VariableGet(
            position.clone(),
            self.resolve_local(name),
        ))
    }

    fn accept_assignment(
        &mut self,
        position: &Position,
        name: &String,
        value: &Expression,
    ) -> Self::Return {
        let value = self.accept_expression(value)?;

        Ok(ResolvedExpression::Assignment(
            position.clone(),
            self.resolve_local(name),
            Box::new(value),
        ))
    }

    fn accept_call(
        &mut self,
        position: &Position,
        callee: &Expression,
        arguments: &[Expression],
    ) -> Self::Return {
        let callee = self.accept_expression(callee)?;
        let arguments = arguments
            .iter()
            .map(|argument| self.accept_expression(argument))
            .collect::<LoxResult<Vec<_>>>()?;

        Ok(ResolvedExpression::Call(
            position.clone(),
            Box::new(callee),
            arguments,
        ))
    }

    fn accept_get(
        &mut self,
        position: &Position,
        expr: &Expression,
        name: &String,
    ) -> Self::Return {
        let expr = self.accept_expression(expr)?;

        Ok(ResolvedExpression::Get(
            position.clone(),
            Box::new(expr),
            name.to_string(),
        ))
    }

    fn accept_set(
        &mut self,
        position: &Position,
        expr: &Expression,
        name: &String,
        value: &Expression,
    ) -> Self::Return {
        let expr = self.accept_expression(expr)?;
        let value = self.accept_expression(value)?;

        Ok(ResolvedExpression::Set(
            position.clone(),
            Box::new(expr),
            name.to_string(),
            Box::new(value),
        ))
    }

    fn accept_this(&mut self, position: &Position, _identifier: &String) -> Self::Return {
        let this = self.resolve_this(position)?;
        Ok(ResolvedExpression::This(position.clone(), this))
    }
}

impl StatementVisitor<String> for Resolver {
    type Return = LoxResult<ResolvedStatement>;

    fn accept_expression_statement(
        &mut self,
        position: &Position,
        expr: &Expression,
    ) -> Self::Return {
        let expr = self.accept_expression(expr)?;

        Ok(ResolvedStatement::Expression(position.clone(), expr))
    }

    fn accept_print_statement(&mut self, position: &Position, expr: &Expression) -> Self::Return {
        let expr = self.accept_expression(expr)?;

        Ok(ResolvedStatement::Print(position.clone(), expr))
    }

    fn accept_var_declaration(
        &mut self,
        position: &Position,
        identifier: &str,
        expr: &Expression,
    ) -> Self::Return {
        self.declare(position, identifier)?;

        let expr = self.accept_expression(expr)?;

        self.define(identifier);

        Ok(ResolvedStatement::VarDeclaration(
            position.clone(),
            identifier.to_string(),
            expr,
        ))
    }

    fn accept_func_declaration(
        &mut self,
        position: &Position,
        func_type: FuncType,
        name: &str,
        parameters: &[String],
        body: &Statement,
    ) -> Self::Return {
        self.declare(position, name)?;
        self.define(name);

        self.begin_scope();

        for param in parameters {
            self.declare(position, param)?;
            self.define(param);
        }

        let body = self.accept_statement(body)?;

        self.end_scope();

        Ok(ResolvedStatement::FuncDeclaration(
            position.clone(),
            func_type,
            name.to_string(),
            parameters.to_vec(),
            Box::new(body),
        ))
    }

    fn accept_class_declaration(
        &mut self,
        position: &Position,
        name: &str,
        methods: &HashMap<String, Statement>,
    ) -> Self::Return {
        self.declare(position, name)?;
        self.define(name);

        self.begin_scope();
        self.declare(position, "this")?;
        self.define("this");

        let original_class_type = self.class_type;
        self.class_type = ClassType::Class;

        let ret = (|| {
            let methods = methods
                .iter()
                .map(|(name, method)| {
                    let method = self.accept_statement(method)?;
                    Ok((name.clone(), method))
                })
                .collect::<LoxResult<HashMap<_, _>>>()?;

            self.end_scope();

            Ok(ResolvedStatement::ClassDeclaration(
                position.clone(),
                name.to_string(),
                methods,
            ))
        })();

        self.class_type = original_class_type;
        ret
    }

    fn accept_block(&mut self, position: &Position, statements: &[Statement]) -> Self::Return {
        self.begin_scope();

        let mut new_statements = Vec::new();

        for stmt in statements {
            new_statements.push(self.accept_statement(stmt)?);
        }

        self.end_scope();

        Ok(ResolvedStatement::Block(position.clone(), new_statements))
    }

    fn accept_if(
        &mut self,
        position: &Position,
        condition: &Expression,
        then_branch: &Statement,
        else_branch: Option<&Statement>,
    ) -> Self::Return {
        let condition = self.accept_expression(condition)?;
        let then_branch = self.accept_statement(then_branch)?;
        let else_branch = else_branch.map(|s| self.accept_statement(s)).transpose()?;

        Ok(ResolvedStatement::If(
            position.clone(),
            condition,
            Box::new(then_branch),
            else_branch.map(Box::new),
        ))
    }

    fn accept_while(
        &mut self,
        position: &Position,
        condition: &Expression,
        body: &Statement,
    ) -> Self::Return {
        let condition = self.accept_expression(condition)?;
        let body = self.accept_statement(body)?;

        Ok(ResolvedStatement::While(
            position.clone(),
            condition,
            Box::new(body),
        ))
    }

    fn accept_return(&mut self, position: &Position, expr: &Expression) -> Self::Return {
        let expr = self.accept_expression(expr)?;

        Ok(ResolvedStatement::Return(position.clone(), expr))
    }
}

pub struct ResolverIterator<Iter> {
    resolver: Resolver,
    iter: Iter,
}

impl<Iter: Iterator<Item = LoxResult<Statement>>> Iterator for ResolverIterator<Iter> {
    type Item = LoxResult<ResolvedStatement>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|r| r.and_then(|s| self.resolver.accept_statement(&s)))
    }
}

pub fn resolve<'a>(
    stmts: impl 'a + IntoIterator<Item = &'a Statement>,
) -> impl Iterator<Item = LoxResult<ResolvedStatement>> + 'a {
    let mut resolver = Resolver::new();
    stmts
        .into_iter()
        .map(move |stmt| resolver.accept_statement(stmt))
}

pub fn resolve_statement(statement: &Statement) -> LoxResult<ResolvedStatement> {
    let mut resolver = Resolver::new();
    resolver.accept_statement(&statement)
}

pub trait Resolvable {
    type Resolver: Iterator<Item = LoxResult<ResolvedStatement>>;

    fn resolve(self) -> Self::Resolver;
}

impl<Iter: IntoIterator<Item = LoxResult<Statement>>> Resolvable for Iter {
    type Resolver = ResolverIterator<Iter::IntoIter>;

    fn resolve(self) -> Self::Resolver {
        ResolverIterator {
            resolver: Resolver::new(),
            iter: self.into_iter(),
        }
    }
}
