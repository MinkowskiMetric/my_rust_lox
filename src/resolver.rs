use crate::{
    BinaryOp, ClassDefinition, CollectedErrors, CollectibleErrors, Expression, ExpressionVisitor,
    FuncDefinition, FuncType, LogicalBinaryOp, LoxError, LoxResult, Position,
    ResolvedClassDefinition, ResolvedExpression, ResolvedFuncDefinition, ResolvedIdentifier,
    ResolvedStatement, Statement, StatementVisitor, UnaryOp, Value,
};
use std::collections::HashMap;

struct Resolver {
    scopes: Vec<HashMap<String, (Position, bool)>>,
    func_type: Option<FuncType>,
}

impl Resolver {
    fn new() -> Self {
        Self {
            scopes: Vec::new(),
            func_type: None,
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

        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), (pos.clone(), false));
        }

        Ok(())
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.get_mut(name).expect("Should declare before define").1 = true;
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
        if self.func_type.map(|t| t.allows_this()).unwrap_or(false) {
            Ok(self.resolve_local("this"))
        } else {
            Err(LoxError::ThisOutsideMethod(pos.clone()))
        }
    }

    fn resolve_super(&mut self, pos: &Position) -> LoxResult<ResolvedIdentifier> {
        if self.func_type.map(|t| t.allows_this()).unwrap_or(false) {
            Ok(self.resolve_local("super"))
        } else {
            Err(LoxError::SuperOutsideMethod(pos.clone()))
        }
    }

    fn resolve_function(
        &mut self,
        position: &Position,
        func_definition: &FuncDefinition,
    ) -> LoxResult<ResolvedFuncDefinition> {
        self.begin_scope();

        let old_function_type = self.func_type.replace(*func_definition.func_type());

        for param in func_definition.parameters() {
            self.declare(position, param)?;
            self.define(param);
        }

        let body = self.accept_statement(func_definition.body())?;

        self.func_type = old_function_type;

        self.end_scope();

        Ok(ResolvedFuncDefinition::new(
            *func_definition.func_type(),
            func_definition.identifier().to_string(),
            func_definition.parameters().to_vec(),
            body,
        ))
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

    fn accept_get(&mut self, position: &Position, expr: &Expression, name: &str) -> Self::Return {
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
        name: &str,
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

    fn accept_this(&mut self, position: &Position, this_identifier: &String) -> Self::Return {
        assert_eq!(this_identifier, "this");

        let this = self.resolve_this(position)?;
        Ok(ResolvedExpression::This(position.clone(), this))
    }

    fn accept_super(
        &mut self,
        position: &Position,
        this_identifier: &String,
        super_identifier: &String,
        name: &str,
    ) -> Self::Return {
        assert_eq!(this_identifier, "this");
        assert_eq!(super_identifier, "super");

        Ok(ResolvedExpression::Super(
            position.clone(),
            self.resolve_this(position)?,
            self.resolve_super(position)?,
            name.to_string(),
        ))
    }

    fn accept_error_expression(&mut self, position: &Position) -> Self::Return {
        Ok(ResolvedExpression::ErrorPlaceholder(position.clone()))
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
        func_definition: &FuncDefinition,
    ) -> Self::Return {
        self.declare(position, func_definition.identifier())?;
        self.define(func_definition.identifier());

        Ok(ResolvedStatement::FuncDeclaration(
            position.clone(),
            self.resolve_function(position, func_definition)?,
        ))
    }

    fn accept_class_declaration(
        &mut self,
        position: &Position,
        class_definition: &ClassDefinition,
    ) -> Self::Return {
        self.declare(position, class_definition.identifier())?;
        self.define(class_definition.identifier());

        let superclass_name = class_definition
            .superclass_identifier()
            .map(|superclass_name| self.resolve_local(superclass_name));

        if superclass_name.is_some() {
            self.begin_scope();
            self.declare(position, "super")?;
            self.define("super");
        }

        self.begin_scope();
        self.declare(position, "this")?;
        self.define("this");

        let methods = class_definition
            .methods()
            .iter()
            .map(|(name, method)| {
                self.resolve_function(position, method)
                    .map(|method| (name.clone(), method))
            })
            .collect::<LoxResult<HashMap<_, _>>>()?;

        self.end_scope();

        if superclass_name.is_some() {
            self.end_scope();
        }

        Ok(ResolvedStatement::ClassDeclaration(
            position.clone(),
            ResolvedClassDefinition::new(
                class_definition.identifier().to_string(),
                superclass_name,
                methods,
            ),
        ))
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

    fn accept_return(&mut self, position: &Position, expr: Option<&Expression>) -> Self::Return {
        let expr = expr.map(|expr| self.accept_expression(expr)).transpose()?;

        if self.func_type == Some(FuncType::Initializer) && expr.is_some() {
            Err(LoxError::ReturnFromInitializer(position.clone()))
        } else {
            Ok(ResolvedStatement::Return(position.clone(), expr))
        }
    }

    fn accept_error_statement(&mut self, position: &Position) -> Self::Return {
        Ok(ResolvedStatement::ErrorPlaceholder(position.clone()))
    }
}

pub struct ResolverIterator<Iter> {
    resolver: Resolver,
    iter: Option<Iter>,
    upstream_errors: Option<Vec<LoxError>>,
    errors: Option<Vec<LoxError>>,
}

impl<Iter: CollectedErrors<Item = Statement>> Iterator for ResolverIterator<Iter> {
    type Item = ResolvedStatement;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.as_mut().and_then(Iter::next) {
                None => {
                    if let Some(iter) = self.iter.take() {
                        self.upstream_errors = iter.errors();
                    }
                    break None;
                }

                Some(stmt) => match self.resolver.accept_statement(&stmt) {
                    Ok(stmt) => break Some(stmt),
                    Err(err) => self.errors.get_or_insert_with(Vec::new).push(err),
                },
            }
        }
    }
}

impl<Iter: CollectedErrors<Item = Statement>> CollectedErrors for ResolverIterator<Iter> {
    fn errors(self) -> Option<Vec<LoxError>> {
        match (self.upstream_errors, self.errors) {
            (Some(mut upstream_errors), Some(mut errors)) => {
                upstream_errors.append(&mut errors);
                Some(upstream_errors)
            }

            (Some(errors), None) | (None, Some(errors)) => Some(errors),

            (None, None) => None,
        }
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
    resolver.accept_statement(statement)
}

pub trait Resolvable {
    type Resolver: Iterator<Item = ResolvedStatement>;

    fn resolve(self) -> Self::Resolver;
}

impl<Iter: CollectibleErrors<Item = Statement>> Resolvable for Iter {
    type Resolver = ResolverIterator<Iter::Collector>;

    fn resolve(self) -> Self::Resolver {
        ResolverIterator {
            resolver: Resolver::new(),
            iter: Some(self.collect_errors()),
            upstream_errors: None,
            errors: None,
        }
    }
}
