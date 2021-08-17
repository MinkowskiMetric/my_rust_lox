use super::Expression;
use crate::{
    BinaryOp, CollectedErrors, CollectibleErrors, FuncType, LogicalBinaryOp, LoxError, Nil,
    Position, PositionedToken, SimpleToken, Statement, Token, UnaryOp, Value,
};
use std::collections::HashMap;

pub struct Parser<Iter: CollectedErrors<Item = PositionedToken>> {
    tokens: Iter,
    next: Option<PositionedToken>,
    last_token_pos: Option<Position>,
    errors: Option<Vec<LoxError>>,
}

macro_rules! binary_expression {
    ($prsr:ident.match_binary_expression($next:ident) { $(SimpleToken::$token:ident => BinaryOp::$op:ident,)+ }) => {
        binary_expression! {
            $prsr.match_internal_binary_expression(Binary, $next) {
                $(SimpleToken::$token => BinaryOp::$op,)+
            }
        }
    };

    ($prsr:ident.match_logical_binary_expression($next:ident) { $(SimpleToken::$token:ident => LogicalBinaryOp::$op:ident,)+ }) => {
        binary_expression! {
            $prsr.match_internal_binary_expression(LogicalBinary, $next) {
                $(SimpleToken::$token => LogicalBinaryOp::$op,)+
            }
        }
    };

    ($prsr:ident.match_internal_binary_expression($expr:ident, $next:ident) { $(SimpleToken::$token:ident => $op_type:ident::$op:ident,)+ }) => {
        let mut left_expr = $prsr.$next();

        loop {
            match $prsr.peek_token() {
                $(Some(Token::Simple(SimpleToken::$token)) => {
                    // Skip past the operator since we already know what it is
                    $prsr.advance_and_discard();

                    let right_expr = $prsr.$next();

                    left_expr = Expression::$expr($prsr.current_position(), Box::new(left_expr), $op_type::$op, Box::new(right_expr));
                },)+

                _ => break left_expr,
            }
        }
    };
}

impl<Iter: CollectedErrors<Item = PositionedToken>> Parser<Iter> {
    fn new(mut iter: Iter) -> Self {
        let next = iter.next();
        Self {
            tokens: iter,
            next,
            last_token_pos: None,
            errors: None,
        }
    }

    fn emit_error(&mut self, error: LoxError) {
        self.errors.get_or_insert_with(Vec::new).push(error)
    }

    fn error_expression(&self) -> Expression {
        assert!(
            self.errors.as_ref().map(Vec::len).unwrap_or(0) > 0,
            "No error has been emitted"
        );
        Expression::ErrorPlaceholder(self.current_position())
    }

    fn error_statement(&self) -> Statement {
        assert!(
            self.errors.as_ref().map(Vec::len).unwrap_or(0) > 0,
            "No error has been emitted"
        );
        Statement::ErrorPlaceholder(self.current_position())
    }

    fn peek(&mut self) -> Option<&PositionedToken> {
        self.next.as_ref()
    }

    fn peek_token(&mut self) -> Option<&Token> {
        self.peek().map(|t| t.token())
    }

    fn advance_token(&mut self) -> Option<Token> {
        let next = std::mem::replace(&mut self.next, self.tokens.next());

        next.map(|t| {
            let (token, token_pos) = t.take();
            self.last_token_pos.replace(token_pos);
            token
        })
    }

    fn advance_and_discard(&mut self) {
        self.advance_token();
    }

    fn advance_identifier(&mut self) -> Option<String> {
        match self.peek_token() {
            Some(Token::Identifier(name)) => {
                let name = name.to_string();
                self.advance_and_discard();
                Some(name)
            }

            _ => {
                self.emit_error(LoxError::ExpectedIdentifier(self.current_position()));
                None
            }
        }
    }

    fn current_position(&self) -> Position {
        self.last_token_pos.clone().unwrap()
    }

    fn map_next_token<R, F: FnOnce(&Token) -> R>(&mut self, f: F) -> Option<R> {
        self.peek_token().map(f)
    }

    fn match_next(&mut self, expected: SimpleToken) -> bool {
        self.map_next_token(|token| matches!(token, Token::Simple(token) if *token == expected))
            .unwrap_or(false)
    }

    fn match_simple(&mut self) -> Option<SimpleToken> {
        self.peek_token().and_then(|token| match token {
            Token::Simple(simple) => Some(*simple),
            _ => None,
        })
    }

    fn consume_next(&mut self, expected: SimpleToken) -> bool {
        if self.match_next(expected) {
            self.advance_and_discard();
            true
        } else {
            false
        }
    }

    fn expect_next(&mut self, expected: SimpleToken) -> bool {
        match self.advance_token() {
            Some(Token::Simple(token)) if token == expected => true,
            Some(unexpected_token) => {
                self.emit_error(LoxError::UnexpectedToken(
                    self.current_position(),
                    unexpected_token,
                ));
                false
            }
            None => {
                self.emit_error(LoxError::UnexpectedEndOfFile(self.current_position()));
                false
            }
        }
    }

    fn expect_and_skip_to(&mut self, expected: SimpleToken) {
        // For use at the end of statements. Allows us to error and skip any garbage at the end of a statement
        if !self.expect_next(expected) {
            self.skip_until(expected);
        }
    }

    fn skip_until(&mut self, target: SimpleToken) {
        loop {
            match self.advance_token() {
                Some(Token::Simple(SimpleToken::EndOfFile)) | None => break,
                Some(Token::Simple(token)) if token == target => break,

                Some(_) => (),
            }
        }
    }

    fn assert_next(&mut self, expected: SimpleToken) {
        assert_eq!(
            self.advance_token(),
            Some(Token::Simple(expected)),
            "This token type should already have been checked - something is very wrong"
        );
    }

    fn declaration(&mut self) -> Statement {
        match self.match_simple() {
            Some(SimpleToken::Var) => self.var_declaration(),
            Some(SimpleToken::Fun) => self.func_declaration(),
            Some(SimpleToken::Class) => self.class_declaration(),
            _ => self.statement(),
        }
    }

    fn var_declaration(&mut self) -> Statement {
        self.assert_next(SimpleToken::Var);

        let ret = self.advance_identifier().map(|identifier| {
            let identifier_pos = self.current_position();

            // Error handling in here is interesting. If we fail to parse an expression, we just behave as if
            // the equals sign wasn't there. Whatever happens we skip until the semicolon.
            let expr = if self.consume_next(SimpleToken::Equal) {
                self.expression()
            } else {
                Expression::Literal(self.current_position(), Value::from(Nil))
            };

            Statement::VarDeclaration(identifier_pos, identifier, expr)
        });

        self.expect_and_skip_to(SimpleToken::Semicolon);

        ret.unwrap_or_else(|| self.error_statement())
    }

    fn func_declaration_impl(&mut self, func_type: FuncType, identifier: String) -> Statement {
        let start_pos = self.current_position();

        if self.expect_next(SimpleToken::LeftParen) {
            let mut parameters = Vec::new();

            if !self.consume_next(SimpleToken::RightParen) {
                loop {
                    parameters.push(match self.advance_identifier() {
                        Some(name) => name,
                        None => {
                            // Skip the token and use a dummy name.
                            self.advance_and_discard();
                            "PARAMETER ERROR".into()
                        }
                    });

                    if !self.consume_next(SimpleToken::Comma) {
                        self.expect_and_skip_to(SimpleToken::RightParen);
                        break;
                    }
                }
            }

            let body = self.block();

            Statement::FuncDeclaration(start_pos, func_type, identifier, parameters, Box::new(body))
        } else {
            self.skip_until(SimpleToken::RightBrace);
            self.error_statement()
        }
    }

    fn func_declaration(&mut self) -> Statement {
        self.assert_next(SimpleToken::Fun);

        match self.advance_identifier() {
            Some(identifier) => self.func_declaration_impl(FuncType::Function, identifier),
            None => self.error_statement(),
        }
    }

    fn method_decaration(&mut self, name: String) -> Statement {
        if name == "init" {
            self.func_declaration_impl(FuncType::Initializer, name)
        } else {
            self.func_declaration_impl(FuncType::Method, name)
        }
    }

    fn class_declaration(&mut self) -> Statement {
        self.assert_next(SimpleToken::Class);
        let start_pos = self.current_position();

        match self.advance_identifier() {
            Some(class_identifier) => {
                let superclass_identifier = if self.consume_next(SimpleToken::Less) {
                    Some(self.advance_identifier().unwrap_or("UNKNOWN CLASS".into()))
                } else {
                    None
                };

                if self.expect_next(SimpleToken::LeftBrace) {
                    let mut methods = HashMap::new();

                    while !self.match_next(SimpleToken::RightBrace) {
                        let method_identifier =
                            self.advance_identifier().unwrap_or("FAKE METHOD".into());
                        methods.insert(
                            method_identifier.clone(),
                            self.method_decaration(method_identifier),
                        );
                    }

                    self.expect_and_skip_to(SimpleToken::RightBrace);

                    Statement::ClassDeclaration(
                        start_pos,
                        class_identifier,
                        superclass_identifier,
                        methods,
                    )
                } else {
                    self.skip_until(SimpleToken::RightBrace);
                    self.error_statement()
                }
            }

            None => self.error_statement(),
        }
    }

    fn statement(&mut self) -> Statement {
        match self.peek_token() {
            Some(Token::Simple(SimpleToken::Print)) => self.print_statement(),
            Some(Token::Simple(SimpleToken::LeftBrace)) => self.block(),
            Some(Token::Simple(SimpleToken::If)) => self.if_statement(),
            Some(Token::Simple(SimpleToken::While)) => self.while_statement(),
            Some(Token::Simple(SimpleToken::For)) => self.for_statement(),
            Some(Token::Simple(SimpleToken::Return)) => self.return_statement(),
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> Statement {
        let expr = self.expression();
        self.expect_and_skip_to(SimpleToken::Semicolon);
        Statement::Expression(self.current_position(), expr)
    }

    fn print_statement(&mut self) -> Statement {
        self.assert_next(SimpleToken::Print);
        let print_pos = self.current_position();

        let expr = self.expression();
        self.expect_and_skip_to(SimpleToken::Semicolon);

        Statement::Print(print_pos, expr)
    }

    fn if_statement(&mut self) -> Statement {
        self.assert_next(SimpleToken::If);
        let if_position = self.current_position();

        self.expect_and_skip_to(SimpleToken::LeftParen);
        let condition = self.expression();
        self.expect_and_skip_to(SimpleToken::RightParen);

        let then_branch = self.statement();

        let else_branch = if self.consume_next(SimpleToken::Else) {
            Some(self.statement())
        } else {
            None
        };

        Statement::If(
            if_position,
            condition,
            Box::new(then_branch),
            else_branch.map(Box::new),
        )
    }

    fn block(&mut self) -> Statement {
        self.assert_next(SimpleToken::LeftBrace);
        let block_pos = self.current_position();
        let mut statements = Vec::new();

        loop {
            if self.consume_next(SimpleToken::RightBrace) {
                break Statement::Block(block_pos, statements);
            } else {
                statements.push(self.declaration());
            }
        }
    }

    fn while_statement(&mut self) -> Statement {
        self.assert_next(SimpleToken::While);

        if self.expect_next(SimpleToken::LeftParen) {
            let condition = self.expression();
            self.expect_and_skip_to(SimpleToken::RightParen);

            Statement::While(
                condition.position().clone(),
                condition,
                Box::new(self.statement()),
            )
        } else {
            self.skip_until(SimpleToken::Semicolon);
            self.error_statement()
        }
    }

    fn for_statement(&mut self) -> Statement {
        self.assert_next(SimpleToken::For);

        if self.expect_next(SimpleToken::LeftParen) {
            let initializer = if self.consume_next(SimpleToken::Semicolon) {
                None
            } else if self.match_next(SimpleToken::Var) {
                Some(self.var_declaration())
            } else {
                Some(self.expression_statement())
            };

            let condition = if self.match_next(SimpleToken::Semicolon) {
                Expression::Literal(self.current_position(), Value::from(true))
            } else {
                self.expression()
            };

            self.expect_and_skip_to(SimpleToken::Semicolon);

            let increment = if self.match_next(SimpleToken::RightParen) {
                None
            } else {
                Some(self.expression())
            };

            self.expect_and_skip_to(SimpleToken::RightParen);

            let body = self.statement();
            let body = match increment {
                Some(increment) => Statement::Block(
                    body.position().clone(),
                    vec![
                        body,
                        Statement::Expression(increment.position().clone(), increment),
                    ],
                ),
                None => body,
            };

            let looper = Statement::While(condition.position().clone(), condition, Box::new(body));

            match initializer {
                Some(initializer) => {
                    Statement::Block(looper.position().clone(), vec![initializer, looper])
                }
                None => looper,
            }
        } else {
            self.skip_until(SimpleToken::RightBrace);
            self.error_statement()
        }
    }

    fn return_statement(&mut self) -> Statement {
        self.assert_next(SimpleToken::Return);
        let start_pos = self.current_position();

        let expr = if self.match_next(SimpleToken::Semicolon) {
            None
        } else {
            Some(self.expression())
        };

        self.expect_and_skip_to(SimpleToken::Semicolon);

        Statement::Return(start_pos, expr)
    }

    fn expression(&mut self) -> Expression {
        self.comma_sequence()
    }

    fn comma_sequence(&mut self) -> Expression {
        binary_expression! { self.match_binary_expression(assignment) {
            SimpleToken::Comma => BinaryOp::Comma,
        } }
    }

    fn assignment(&mut self) -> Expression {
        match self.logic_or() {
            Expression::VariableGet(_, identifier) if self.consume_next(SimpleToken::Equal) => {
                Expression::Assignment(
                    self.current_position(),
                    identifier.to_string(),
                    Box::new(self.assignment()),
                )
            }

            Expression::Get(_, expr, identifier) if self.consume_next(SimpleToken::Equal) => {
                Expression::Set(
                    self.current_position(),
                    expr,
                    identifier.to_string(),
                    Box::new(self.assignment()),
                )
            }

            expr => expr,
        }
    }

    fn logic_or(&mut self) -> Expression {
        binary_expression! { self.match_logical_binary_expression(logic_and) {
            SimpleToken::Or => LogicalBinaryOp::Or,
        } }
    }

    fn logic_and(&mut self) -> Expression {
        binary_expression! { self.match_logical_binary_expression(equality) {
            SimpleToken::And => LogicalBinaryOp::And,
        } }
    }

    fn equality(&mut self) -> Expression {
        binary_expression! { self.match_binary_expression(ternary) {
            SimpleToken::EqualEqual => BinaryOp::EqualEqual,
            SimpleToken::BangEqual => BinaryOp::BangEqual,
        } }
    }

    fn ternary(&mut self) -> Expression {
        let comparison_expr = self.comparison();

        if self.consume_next(SimpleToken::QuestionMark) {
            let true_expr = self.expression();
            self.expect_and_skip_to(SimpleToken::Colon);
            let false_expr = self.expression();

            Expression::Ternary(
                self.current_position(),
                Box::new(comparison_expr),
                Box::new(true_expr),
                Box::new(false_expr),
            )
        } else {
            comparison_expr
        }
    }

    fn comparison(&mut self) -> Expression {
        binary_expression! { self.match_binary_expression(term) {
            SimpleToken::Greater => BinaryOp::Greater,
            SimpleToken::GreaterEqual => BinaryOp::GreaterEqual,
            SimpleToken::Less => BinaryOp::Less,
            SimpleToken::LessEqual => BinaryOp::LessEqual,
        } }
    }

    fn term(&mut self) -> Expression {
        binary_expression! { self.match_binary_expression(factor) {
            SimpleToken::Plus => BinaryOp::Plus,
            SimpleToken::Minus => BinaryOp::Minus,
        } }
    }

    fn factor(&mut self) -> Expression {
        binary_expression! { self.match_binary_expression(unary) {
            SimpleToken::Star => BinaryOp::Star,
            SimpleToken::Slash => BinaryOp::Slash,
        } }
    }

    fn unary(&mut self) -> Expression {
        if self.consume_next(SimpleToken::Minus) {
            let position = self.current_position();
            Expression::Unary(position, UnaryOp::Minus, Box::new(self.unary()))
        } else if self.consume_next(SimpleToken::Bang) {
            let position = self.current_position();
            Expression::Unary(position, UnaryOp::Bang, Box::new(self.unary()))
        } else if self.consume_next(SimpleToken::LeftParen) {
            let position = self.current_position();
            let expr = self.expression();
            self.expect_and_skip_to(SimpleToken::RightParen);

            Expression::Unary(position, UnaryOp::Grouping, Box::new(expr))
        } else {
            self.call()
        }
    }

    fn finish_call(&mut self, callee: Expression) -> Expression {
        let mut arguments = Vec::new();

        if !self.consume_next(SimpleToken::RightParen) {
            loop {
                // Parse a single argument - use assignment so we don't parse comma sequences
                arguments.push(self.assignment());

                if !self.consume_next(SimpleToken::Comma) {
                    self.expect_and_skip_to(SimpleToken::RightParen);

                    break Expression::Call(self.current_position(), Box::new(callee), arguments);
                }
            }
        } else {
            Expression::Call(self.current_position(), Box::new(callee), arguments)
        }
    }

    fn call(&mut self) -> Expression {
        let mut expr = self.primary();

        loop {
            if self.consume_next(SimpleToken::LeftParen) {
                expr = self.finish_call(expr);
            } else if self.consume_next(SimpleToken::Dot) {
                match self.advance_identifier() {
                    Some(identifier) => {
                        expr = Expression::Get(self.current_position(), Box::new(expr), identifier);
                    }
                    None => {
                        // Return what we have and hope we can go on
                        break self.error_expression();
                    }
                }
            } else {
                break expr;
            }
        }
    }

    fn primary(&mut self) -> Expression {
        match self.advance_token() {
            Some(Token::Literal(value)) => Expression::Literal(self.current_position(), value),
            Some(Token::Identifier(name)) => Expression::VariableGet(self.current_position(), name),
            Some(Token::Simple(SimpleToken::Nil)) => {
                Expression::Literal(self.current_position(), Nil.into())
            }
            Some(Token::Simple(SimpleToken::True)) => {
                Expression::Literal(self.current_position(), true.into())
            }
            Some(Token::Simple(SimpleToken::False)) => {
                Expression::Literal(self.current_position(), false.into())
            }
            Some(Token::Simple(SimpleToken::This)) => {
                Expression::This(self.current_position(), "this".to_string())
            }
            Some(Token::Simple(SimpleToken::Super)) => {
                if self.expect_next(SimpleToken::Dot) {
                    match self.advance_identifier() {
                        Some(identifier) => Expression::Super(
                            self.current_position(),
                            "this".to_string(),
                            "super".to_string(),
                            identifier,
                        ),

                        None => self.error_expression(),
                    }
                } else {
                    // Treat super without a dot as an access to a variable
                    // called super, which obviously can't exist.
                    self.emit_error(LoxError::ExpectedToken(
                        self.current_position(),
                        SimpleToken::Dot,
                    ));
                    Expression::VariableGet(self.current_position(), "super".to_string())
                }
            }
            Some(token) => {
                self.emit_error(LoxError::UnexpectedToken(self.current_position(), token));
                self.error_expression()
            }
            None => panic!("End of file token missing"),
        }
    }
}

impl<Iter: CollectedErrors<Item = PositionedToken>> Iterator for Parser<Iter> {
    type Item = Statement;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek().map(|t| t.token()) {
            Some(Token::Simple(SimpleToken::EndOfFile)) | None => None,
            _ => Some(self.declaration()),
        }
    }
}

impl<Iter: CollectedErrors<Item = PositionedToken>> CollectedErrors for Parser<Iter> {
    fn errors(self) -> Option<Vec<LoxError>> {
        match (self.tokens.errors(), self.errors) {
            (Some(mut token_errors), Some(mut errors)) => {
                token_errors.append(&mut errors);
                Some(token_errors)
            }

            (Some(errors), None) | (None, Some(errors)) => Some(errors),

            (None, None) => None,
        }
    }
}

pub fn parse(
    tokens: impl CollectibleErrors<Item = PositionedToken>,
) -> impl CollectedErrors<Item = Statement> {
    Parser::new(tokens.collect_errors())
}

pub trait Parseable {
    type Parser: CollectedErrors<Item = Statement>;

    fn parse(self) -> Self::Parser;
}

impl<Iter: CollectibleErrors<Item = PositionedToken>> Parseable for Iter {
    type Parser = Parser<Iter::Collector>;

    fn parse(self) -> Self::Parser {
        Self::Parser::new(self.collect_errors())
    }
}
