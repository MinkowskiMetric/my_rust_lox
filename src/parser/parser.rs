use super::Expression;
use crate::{
    BinaryOp, LogicalBinaryOp, LoxError, LoxResult, Nil, Position, PositionedToken, SimpleToken,
    Statement, Token, UnaryOp, Value,
};
use std::iter::Peekable;

pub struct Parser<Iter: Iterator<Item = PositionedToken>> {
    tokens: Peekable<Iter>,
    last_token_pos: Option<Position>,
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
        let mut left_expr = $prsr.$next()?;

        loop {
            match $prsr.peek_token() {
                $(Some(Token::Simple(SimpleToken::$token)) => {
                    // Skip past the operator since we already know what it is
                    $prsr.advance_and_discard();

                    let right_expr = $prsr.$next()?;

                    left_expr = Expression::$expr($prsr.current_position(), Box::new(left_expr), $op_type::$op, Box::new(right_expr));
                },)+

                _ => break Ok(left_expr),
            };
        }
    };
}

impl<Iter: Iterator<Item = PositionedToken>> Parser<Iter> {
    fn new(iter: Iter) -> Self {
        Self {
            tokens: iter.peekable(),
            last_token_pos: None,
        }
    }

    fn peek(&mut self) -> Option<&PositionedToken> {
        self.tokens.peek()
    }

    fn peek_token(&mut self) -> Option<&Token> {
        self.peek().map(|t| t.token())
    }

    fn advance_token(&mut self) -> Option<Token> {
        let next = self.tokens.next();

        next.map(|t| {
            let (token, token_pos) = t.take();
            self.last_token_pos.replace(token_pos);
            token
        })
    }

    fn advance_and_discard(&mut self) {
        self.advance_token();
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

    fn consume_next(&mut self, expected: SimpleToken) -> bool {
        if self.match_next(expected) {
            self.advance_and_discard();
            true
        } else {
            false
        }
    }

    fn expect_next(&mut self, expected: SimpleToken) -> LoxResult<()> {
        match self.advance_token() {
            Some(Token::Simple(token)) if token == expected => Ok(()),
            Some(Token::Simple(SimpleToken::EndOfFile)) => {
                Err(LoxError::IncompleteExpression(self.current_position()))
            }
            Some(unexpected_token) => Err(LoxError::UnexpectedToken(
                unexpected_token,
                self.current_position(),
            )),
            None => panic!("End of file token missing"),
        }
    }

    fn assert_next(&mut self, expected: SimpleToken) {
        assert_eq!(
            self.advance_token(),
            Some(Token::Simple(expected)),
            "This token type should already have been checked - something is very wrong"
        );
    }

    fn declaration(&mut self) -> LoxResult<Statement> {
        if self.match_next(SimpleToken::Var) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> LoxResult<Statement> {
        self.assert_next(SimpleToken::Var);

        let name = match self.advance_token() {
            Some(Token::Identifier(name)) => name,
            _ => return Err(LoxError::MissingIdentifier(self.current_position())),
        };
        let name_pos = self.current_position();

        let expr = if self.consume_next(SimpleToken::Equal) {
            self.expression()?
        } else {
            Expression::Literal(self.current_position(), Value::from(Nil))
        };

        self.expect_next(SimpleToken::Semicolon)?;

        Ok(Statement::VarDeclaration(name_pos, name, expr))
    }

    fn statement(&mut self) -> LoxResult<Statement> {
        match self.peek_token() {
            Some(Token::Simple(SimpleToken::Print)) => self.print_statement(),
            Some(Token::Simple(SimpleToken::LeftBrace)) => self.block(),
            Some(Token::Simple(SimpleToken::If)) => self.if_statement(),
            Some(Token::Simple(SimpleToken::While)) => self.while_statement(),
            Some(Token::Simple(SimpleToken::For)) => self.for_statement(),
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> LoxResult<Statement> {
        let expr = self.expression()?;
        self.expect_next(SimpleToken::Semicolon)?;

        Ok(Statement::Expression(self.current_position(), expr))
    }

    fn print_statement(&mut self) -> LoxResult<Statement> {
        self.assert_next(SimpleToken::Print);
        let print_pos = self.current_position();

        let expr = self.expression()?;
        self.expect_next(SimpleToken::Semicolon)?;

        Ok(Statement::Print(print_pos, expr))
    }

    fn if_statement(&mut self) -> LoxResult<Statement> {
        self.assert_next(SimpleToken::If);
        let if_position = self.current_position();

        self.expect_next(SimpleToken::LeftParen)?;

        let condition = self.expression()?;

        self.expect_next(SimpleToken::RightParen)?;

        let then_branch = self.statement()?;

        if self.consume_next(SimpleToken::Else) {
            let else_branch = self.statement()?;

            Ok(Statement::If(
                if_position,
                condition,
                Box::new(then_branch),
                Some(Box::new(else_branch)),
            ))
        } else {
            Ok(Statement::If(
                if_position,
                condition,
                Box::new(then_branch),
                None,
            ))
        }
    }

    fn block(&mut self) -> LoxResult<Statement> {
        self.assert_next(SimpleToken::LeftBrace);
        let block_pos = self.current_position();
        let mut statements = Vec::new();

        loop {
            if self.consume_next(SimpleToken::RightBrace) {
                break Ok(Statement::Block(block_pos, statements));
            } else {
                statements.push(self.declaration()?);
            }
        }
    }

    fn while_statement(&mut self) -> LoxResult<Statement> {
        self.assert_next(SimpleToken::While);

        self.expect_next(SimpleToken::LeftParen)?;

        let condition = self.expression()?;

        self.expect_next(SimpleToken::RightParen)?;

        let body = self.statement()?;

        Ok(Statement::While(
            condition.position().clone(),
            condition,
            Box::new(body),
        ))
    }

    fn for_statement(&mut self) -> LoxResult<Statement> {
        self.assert_next(SimpleToken::For);

        self.expect_next(SimpleToken::LeftParen)?;

        let initializer = if self.consume_next(SimpleToken::Semicolon) {
            None
        } else if self.match_next(SimpleToken::Var) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if self.match_next(SimpleToken::Semicolon) {
            // If no condition is specified, loop forever
            Expression::Literal(self.current_position(), Value::from(true))
        } else {
            self.expression()?
        };

        self.expect_next(SimpleToken::Semicolon)?;

        let increment = if self.match_next(SimpleToken::RightParen) {
            None
        } else {
            Some(self.expression()?)
        };

        self.expect_next(SimpleToken::RightParen)?;

        let body = self.statement()?;
        let body = if let Some(increment) = increment {
            Statement::Block(
                body.position().clone(),
                vec![
                    body,
                    Statement::Expression(increment.position().clone(), increment),
                ],
            )
        } else {
            body
        };

        let looper = Statement::While(condition.position().clone(), condition, Box::new(body));

        if let Some(initializer) = initializer {
            Ok(Statement::Block(
                looper.position().clone(),
                vec![initializer, looper],
            ))
        } else {
            Ok(looper)
        }
    }

    fn expression(&mut self) -> LoxResult<Expression> {
        self.comma_sequence()
    }

    fn comma_sequence(&mut self) -> LoxResult<Expression> {
        binary_expression! { self.match_binary_expression(assignment) {
            SimpleToken::Comma => BinaryOp::Comma,
        } }
    }

    fn assignment(&mut self) -> LoxResult<Expression> {
        let expr = self.logic_or()?;

        match &expr {
            Expression::VariableGet(_, name) => {
                if self.consume_next(SimpleToken::Equal) {
                    let value = self.assignment()?;

                    Ok(Expression::Assignment(
                        self.current_position(),
                        name.to_string(),
                        Box::new(value),
                    ))
                } else {
                    Ok(expr)
                }
            }

            _ => Ok(expr),
        }
    }

    fn logic_or(&mut self) -> LoxResult<Expression> {
        binary_expression! { self.match_logical_binary_expression(logic_and) {
            SimpleToken::Or => LogicalBinaryOp::Or,
        } }
    }

    fn logic_and(&mut self) -> LoxResult<Expression> {
        binary_expression! { self.match_logical_binary_expression(equality) {
            SimpleToken::And => LogicalBinaryOp::And,
        } }
    }

    fn equality(&mut self) -> LoxResult<Expression> {
        binary_expression! { self.match_binary_expression(ternary) {
            SimpleToken::EqualEqual => BinaryOp::EqualEqual,
            SimpleToken::BangEqual => BinaryOp::BangEqual,
        } }
    }

    fn ternary(&mut self) -> LoxResult<Expression> {
        let comparison_expr = self.comparison()?;

        if self.consume_next(SimpleToken::QuestionMark) {
            let true_expr = self.expression()?;

            self.expect_next(SimpleToken::Colon)?;

            let false_expr = self.expression()?;

            Ok(Expression::Ternary(
                self.current_position(),
                Box::new(comparison_expr),
                Box::new(true_expr),
                Box::new(false_expr),
            ))
        } else {
            Ok(comparison_expr)
        }
    }

    fn comparison(&mut self) -> LoxResult<Expression> {
        binary_expression! { self.match_binary_expression(term) {
            SimpleToken::Greater => BinaryOp::Greater,
            SimpleToken::GreaterEqual => BinaryOp::GreaterEqual,
            SimpleToken::Less => BinaryOp::Less,
            SimpleToken::LessEqual => BinaryOp::LessEqual,
        } }
    }

    fn term(&mut self) -> LoxResult<Expression> {
        binary_expression! { self.match_binary_expression(factor) {
            SimpleToken::Plus => BinaryOp::Plus,
            SimpleToken::Minus => BinaryOp::Minus,
        } }
    }

    fn factor(&mut self) -> LoxResult<Expression> {
        binary_expression! { self.match_binary_expression(unary) {
            SimpleToken::Star => BinaryOp::Star,
            SimpleToken::Slash => BinaryOp::Slash,
        } }
    }

    fn unary(&mut self) -> LoxResult<Expression> {
        if self.consume_next(SimpleToken::Minus) {
            Ok(Expression::Unary(
                self.current_position(),
                UnaryOp::Minus,
                Box::new(self.unary()?),
            ))
        } else if self.consume_next(SimpleToken::Bang) {
            Ok(Expression::Unary(
                self.current_position(),
                UnaryOp::Bang,
                Box::new(self.unary()?),
            ))
        } else if self.consume_next(SimpleToken::LeftParen) {
            let ret = Expression::Unary(
                self.current_position(),
                UnaryOp::Grouping,
                Box::new(self.expression()?),
            );
            self.expect_next(SimpleToken::RightParen)?;
            Ok(ret)
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> LoxResult<Expression> {
        let callee = self.primary()?;

        if self.consume_next(SimpleToken::LeftParen) {
            let mut arguments = Vec::new();

            if !self.match_next(SimpleToken::RightParen) {
                loop {
                    // Parse a single argument - use assignment so we don't parse comma sequences
                    arguments.push(self.assignment()?);

                    // If there is no comma, we're done
                    if !self.consume_next(SimpleToken::Comma) {
                        break;
                    }
                }
            }

            self.expect_next(SimpleToken::RightParen)?;

            Ok(Expression::Call(
                self.current_position(),
                Box::new(callee),
                arguments,
            ))
        } else {
            Ok(callee)
        }
    }

    fn primary(&mut self) -> LoxResult<Expression> {
        match self.advance_token() {
            Some(Token::Literal(value)) => Ok(Expression::Literal(self.current_position(), value)),
            Some(Token::Identifier(name)) => {
                Ok(Expression::VariableGet(self.current_position(), name))
            }
            Some(Token::Simple(SimpleToken::Nil)) => {
                Ok(Expression::Literal(self.current_position(), Nil.into()))
            }
            Some(Token::Simple(SimpleToken::True)) => {
                Ok(Expression::Literal(self.current_position(), true.into()))
            }
            Some(Token::Simple(SimpleToken::False)) => {
                Ok(Expression::Literal(self.current_position(), false.into()))
            }
            Some(Token::Simple(SimpleToken::EndOfFile)) => {
                Err(LoxError::IncompleteExpression(self.current_position()))
            }
            Some(token) => Err(LoxError::UnexpectedToken(token, self.current_position())),
            None => panic!("End of file token missing"),
        }
    }
}

impl<Iter: Iterator<Item = PositionedToken>> Iterator for Parser<Iter> {
    type Item = LoxResult<Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek().map(|t| t.token()) {
            Some(Token::Simple(SimpleToken::EndOfFile)) | None => None,
            _ => Some(self.declaration()),
        }
    }
}

pub fn parse(
    tokens: impl IntoIterator<Item = PositionedToken>,
) -> impl Iterator<Item = LoxResult<Statement>> {
    Parser::new(tokens.into_iter())
}
