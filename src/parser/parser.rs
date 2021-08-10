use super::Expression;
use crate::{
    BinaryOp, LogicalBinaryOp, LoxError, LoxResult, Nil, Position, PositionTagged, SimpleToken,
    Statement, Token, UnaryOp, Value,
};
use std::iter::Peekable;

pub struct Parser<Iter: Iterator<Item = PositionTagged<Token>>> {
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
                    $prsr.advance();

                    let right_expr = $prsr.$next()?;

                    left_expr = Expression::$expr(Box::new(left_expr), $op_type::$op, Box::new(right_expr));
                },)+

                _ => break Ok(left_expr),
            };
        }
    };
}

impl<Iter: Iterator<Item = PositionTagged<Token>>> Parser<Iter> {
    fn new(iter: Iter) -> Self {
        Self {
            tokens: iter.peekable(),
            last_token_pos: None,
        }
    }

    fn peek(&mut self) -> Option<&PositionTagged<Token>> {
        self.tokens.peek()
    }

    fn peek_token(&mut self) -> Option<&Token> {
        self.peek().map(|t| t.value())
    }

    fn advance_token(&mut self) -> Option<Token> {
        let next = self.tokens.next();

        next.map(|t| {
            let (token, token_pos) = t.take();
            self.last_token_pos.replace(token_pos);
            token
        })
    }

    fn advance(&mut self) -> Option<PositionTagged<Token>> {
        self.advance_token()
            .map(|t| PositionTagged::new(t, self.current_position()))
    }

    fn current_position(&self) -> Position {
        self.last_token_pos.clone().unwrap()
    }

    fn next_position(&mut self) -> Position {
        // There is a possibility that we don't have a next token because we're at the end.
        // To avoid complexity, we just use the last position in that case
        self.peek()
            .map(|v| v.position().clone())
            .unwrap_or_else(|| self.current_position())
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
            self.advance();
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

    fn parse_expression<F: FnOnce(&mut Self) -> LoxResult<Expression>>(
        &mut self,
        f: F,
    ) -> LoxResult<PositionTagged<Expression>> {
        let start_pos = self.next_position();

        let expression = f(self)?;
        let end_pos = self.current_position();

        Ok(PositionTagged::new_from_to(expression, start_pos, end_pos))
    }

    fn parse_statement<F: FnOnce(&mut Self) -> LoxResult<Statement>>(
        &mut self,
        start_token: SimpleToken,
        f: F,
    ) -> LoxResult<PositionTagged<Statement>> {
        assert!(self.consume_next(start_token), "Invalid start token");
        let start_pos = self.next_position();

        let statement = f(self)?;
        let end_pos = self.current_position();

        Ok(PositionTagged::new_from_to(statement, start_pos, end_pos))
    }

    fn declaration(&mut self) -> LoxResult<PositionTagged<Statement>> {
        if self.match_next(SimpleToken::Var) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> LoxResult<PositionTagged<Statement>> {
        self.parse_statement(SimpleToken::Var, |parser| {
            let name = match parser.advance_token() {
                Some(Token::Identifier(name)) => name,
                _ => return Err(LoxError::MissingIdentifier(parser.current_position())),
            };

            let expr = if parser.consume_next(SimpleToken::Equal) {
                parser.expression()?.take().0
            } else {
                Expression::Literal(Value::from(Nil))
            };

            parser.expect_next(SimpleToken::Semicolon)?;

            Ok(Statement::VarDeclaration(name, expr))
        })
    }

    fn statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        match self.peek_token() {
            Some(Token::Simple(SimpleToken::Print)) => self.print_statement(),
            Some(Token::Simple(SimpleToken::LeftBrace)) => self.block(),
            Some(Token::Simple(SimpleToken::If)) => self.if_statement(),
            Some(Token::Simple(SimpleToken::While)) => self.while_statement(),
            Some(Token::Simple(SimpleToken::For)) => self.for_statement(),
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        let (expr, expr_pos) = self.expression()?.take();
        self.expect_next(SimpleToken::Semicolon)?;

        Ok(PositionTagged::new_from_to(
            Statement::Expression(expr),
            expr_pos,
            self.current_position(),
        ))
    }

    fn print_statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        assert!(
            self.consume_next(SimpleToken::Print),
            "Expected to be called on print token"
        );
        let print_pos = self.current_position();

        let (expr, _) = self.expression()?.take();
        self.expect_next(SimpleToken::Semicolon)?;

        Ok(PositionTagged::new_from_to(
            Statement::Print(expr),
            print_pos,
            self.current_position(),
        ))
    }

    fn if_statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        self.parse_statement(SimpleToken::If, |parser| {
            parser.expect_next(SimpleToken::LeftParen)?;

            let (condition, _) = parser.expression()?.take();

            parser.expect_next(SimpleToken::RightParen)?;

            let (then_branch, _) = parser.statement()?.take();

            if parser.consume_next(SimpleToken::Else) {
                let (else_branch, _) = parser.statement()?.take();

                Ok(Statement::If(
                    condition,
                    Box::new(then_branch),
                    Some(Box::new(else_branch)),
                ))
            } else {
                Ok(Statement::If(condition, Box::new(then_branch), None))
            }
        })
    }

    fn block(&mut self) -> LoxResult<PositionTagged<Statement>> {
        self.parse_statement(SimpleToken::LeftBrace, |parser| {
            let mut statements = Vec::new();

            loop {
                if parser.consume_next(SimpleToken::RightBrace) {
                    break Ok(Statement::Block(statements));
                } else {
                    statements.push(parser.declaration()?.take().0);
                }
            }
        })
    }

    fn while_statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        self.parse_statement(SimpleToken::While, |parser| {
            parser.expect_next(SimpleToken::LeftParen)?;

            let (condition, _) = parser.expression()?.take();

            parser.expect_next(SimpleToken::RightParen)?;

            let (body, _) = parser.statement()?.take();

            Ok(Statement::While(condition, Box::new(body)))
        })
    }

    fn for_statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        self.parse_statement(SimpleToken::For, |parser| {
            parser.expect_next(SimpleToken::LeftParen)?;

            let initializer = if parser.consume_next(SimpleToken::Semicolon) {
                None
            } else if parser.match_next(SimpleToken::Var) {
                Some(parser.var_declaration()?.take().0)
            } else {
                Some(parser.expression_statement()?.take().0)
            };

            let condition = if parser.match_next(SimpleToken::Semicolon) {
                // If no condition is specified, loop forever
                Expression::Literal(Value::from(true))
            } else {
                parser.expression()?.take().0
            };

            parser.expect_next(SimpleToken::Semicolon)?;

            let increment = if parser.match_next(SimpleToken::RightParen) {
                None
            } else {
                Some(parser.expression()?.take().0)
            };

            parser.expect_next(SimpleToken::RightParen)?;

            let body = parser.statement()?.take().0;
            let body = if let Some(increment) = increment {
                Statement::Block(vec![body, Statement::Expression(increment)])
            } else {
                body
            };

            let looper = Statement::While(condition, Box::new(body));

            if let Some(initializer) = initializer {
                Ok(Statement::Block(vec![initializer, looper]))
            } else {
                Ok(looper)
            }
        })
    }

    fn expression(&mut self) -> LoxResult<PositionTagged<Expression>> {
        self.parse_expression(Self::comma_sequence)
    }

    fn comma_sequence(&mut self) -> LoxResult<Expression> {
        binary_expression! { self.match_binary_expression(assignment) {
            SimpleToken::Comma => BinaryOp::Comma,
        } }
    }

    fn assignment(&mut self) -> LoxResult<Expression> {
        let expr = self.logic_or()?;

        match &expr {
            Expression::VariableGet(name) => {
                if self.consume_next(SimpleToken::Equal) {
                    let value = self.assignment()?;

                    Ok(Expression::Assignment(name.to_string(), Box::new(value)))
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
            let true_expr = self.expression()?.take().0;

            self.expect_next(SimpleToken::Colon)?;

            let false_expr = self.expression()?.take().0;

            Ok(Expression::Ternary(
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
            Ok(Expression::Unary(UnaryOp::Minus, Box::new(self.unary()?)))
        } else if self.consume_next(SimpleToken::Bang) {
            Ok(Expression::Unary(UnaryOp::Bang, Box::new(self.unary()?)))
        } else if self.consume_next(SimpleToken::LeftParen) {
            let ret = Expression::Unary(UnaryOp::Grouping, Box::new(self.expression()?.take().0));
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

            Ok(Expression::Call(Box::new(callee), arguments))
        } else {
            Ok(callee)
        }
    }

    fn primary(&mut self) -> LoxResult<Expression> {
        match self.advance_token() {
            Some(Token::Literal(value)) => Ok(Expression::Literal(value)),
            Some(Token::Identifier(name)) => Ok(Expression::VariableGet(name)),
            Some(Token::Simple(SimpleToken::Nil)) => Ok(Expression::Literal(Nil.into())),
            Some(Token::Simple(SimpleToken::True)) => Ok(Expression::Literal(true.into())),
            Some(Token::Simple(SimpleToken::False)) => Ok(Expression::Literal(false.into())),
            Some(Token::Simple(SimpleToken::EndOfFile)) => {
                Err(LoxError::IncompleteExpression(self.current_position()))
            }
            Some(token) => Err(LoxError::UnexpectedToken(token, self.current_position())),
            None => panic!("End of file token missing"),
        }
    }
}

impl<Iter: Iterator<Item = PositionTagged<Token>>> Iterator for Parser<Iter> {
    type Item = LoxResult<PositionTagged<Statement>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek().map(|t| t.value()) {
            Some(Token::Simple(SimpleToken::EndOfFile)) | None => None,
            _ => Some(self.declaration()),
        }
    }
}

pub fn parse(
    tokens: impl IntoIterator<Item = PositionTagged<Token>>,
) -> impl Iterator<Item = LoxResult<PositionTagged<Statement>>> {
    Parser::new(tokens.into_iter())
}
