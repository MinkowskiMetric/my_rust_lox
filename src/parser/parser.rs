use super::Expression;
use crate::{
    BinaryOp, LogicalBinaryOp, LoxError, LoxResult, Nil, Position, PositionTagged, Statement,
    Token, UnaryOp, Value,
};
use std::iter::Peekable;

pub struct Parser<Iter: Iterator<Item = PositionTagged<Token>>> {
    tokens: Peekable<Iter>,
}

macro_rules! binary_expression {
    ($prsr:ident.match_binary_expression($next:ident) { $(Token::$token:ident => BinaryOp::$op:ident,)+ }) => {
        binary_expression! {
            $prsr.match_internal_binary_expression(Binary, $next) {
                $(Token::$token => BinaryOp::$op,)+
            }
        }
    };

    ($prsr:ident.match_logical_binary_expression($next:ident) { $(Token::$token:ident => LogicalBinaryOp::$op:ident,)+ }) => {
        binary_expression! {
            $prsr.match_internal_binary_expression(LogicalBinary, $next) {
                $(Token::$token => LogicalBinaryOp::$op,)+
            }
        }
    };

    ($prsr:ident.match_internal_binary_expression($expr:ident, $next:ident) { $(Token::$token:ident => $op_type:ident::$op:ident,)+ }) => {
        let mut left = $prsr.$next()?;

        loop {
            match $prsr.peek().map(|t| t.value()) {
                $(Some(Token::$token) => {
                    // Skip past the operator since we already know what it is
                    $prsr.advance();

                    let (left_expr, left_expr_pos) = left.take();
                    let (right_expr, right_expr_pos) = $prsr.$next()?.take();

                    left = PositionTagged::new(
                        Expression::$expr(Box::new(left_expr), $op_type::$op, Box::new(right_expr)),
                        Position::new(
                            left_expr_pos.file_name().clone(),
                            *left_expr_pos.start(),
                            *right_expr_pos.end(),
                        )
                    );
                },)+

                _ => break,
            };
        }

        Ok(left)
    };
}

impl<Iter: Iterator<Item = PositionTagged<Token>>> Parser<Iter> {
    fn new(iter: Iter) -> Self {
        Self {
            tokens: iter.peekable(),
        }
    }

    fn peek(&mut self) -> Option<&PositionTagged<Token>> {
        self.tokens.peek()
    }

    fn advance(&mut self) -> Option<PositionTagged<Token>> {
        self.tokens.next()
    }

    fn advance_if<R>(&mut self, f: impl FnOnce(&Token) -> Option<R>) -> Option<PositionTagged<R>> {
        match self
            .peek()
            .and_then(|pt| f(pt.value()).map(|r| PositionTagged::new(r, pt.position().clone())))
        {
            Some(r) => {
                self.advance();
                Some(r)
            }
            None => None,
        }
    }

    fn match_exact_token(&mut self, token: Token) -> LoxResult<(Token, Position)> {
        match self.advance().map(|t| t.take()) {
            Some((check_token, pos)) if check_token == token => Ok((check_token, pos)),

            Some((Token::EndOfFile, pos)) => Err(LoxError::IncompleteExpression(pos)),
            Some((check_token, pos)) => Err(LoxError::UnexpectedToken(check_token, pos)),
            None => panic!("End of file token missing"),
        }
    }

    fn declaration(&mut self) -> LoxResult<PositionTagged<Statement>> {
        match self.peek().map(|t| t.value()) {
            Some(Token::Var) => self.var_declaration(),
            _ => self.statement(),
        }
    }

    fn var_declaration(&mut self) -> LoxResult<PositionTagged<Statement>> {
        let (_, declare_pos) = self.match_exact_token(Token::Var)?;

        let (name, _) = self
            .advance_if(|t| match t {
                Token::Identifier(name) => Some(name.clone()),
                _ => None,
            })
            .ok_or(LoxError::MissingIdentifier(declare_pos.clone()))?
            .take();

        let expr = if self
            .advance_if(|t| if *t == Token::Equal { Some(()) } else { None })
            .is_some()
        {
            self.expression()?.take().0
        } else {
            Expression::Literal(Value::from(Nil))
        };

        let (_, semi_pos) = self.match_exact_token(Token::Semicolon)?;

        Ok(PositionTagged::new_from_to(
            Statement::VarDeclaration(name, expr),
            declare_pos,
            semi_pos,
        ))
    }

    fn statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        match self.peek().map(|t| t.value()) {
            Some(Token::Print) => self.print_statement(),
            Some(Token::LeftBrace) => self.block(),
            Some(Token::If) => self.if_statement(),
            Some(Token::While) => self.while_statement(),
            Some(Token::For) => self.for_statement(),
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        let (expr, expr_pos) = self.expression()?.take();
        let (_, semi_pos) = self.match_exact_token(Token::Semicolon)?;

        Ok(PositionTagged::new_from_to(
            Statement::Expression(expr),
            expr_pos,
            semi_pos,
        ))
    }

    fn print_statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        let (_, print_pos) = self.match_exact_token(Token::Print)?;
        let (expr, _) = self.expression()?.take();
        let (_, semi_pos) = self.match_exact_token(Token::Semicolon)?;

        Ok(PositionTagged::new_from_to(
            Statement::Print(expr),
            print_pos,
            semi_pos,
        ))
    }

    fn if_statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        let (_, if_pos) = self.match_exact_token(Token::If)?;
        self.match_exact_token(Token::LeftParen)?;

        let (condition, _) = self.expression()?.take();
        self.match_exact_token(Token::RightParen)?;

        let (then_branch, then_branch_pos) = self.statement()?.take();

        match self.peek().map(|v| v.value()) {
            Some(Token::Else) => {
                self.match_exact_token(Token::Else)?;
                let (else_branch, else_branch_pos) = self.statement()?.take();

                Ok(PositionTagged::new_from_to(
                    Statement::If(
                        condition,
                        Box::new(then_branch),
                        Some(Box::new(else_branch)),
                    ),
                    if_pos,
                    else_branch_pos,
                ))
            }

            _ => Ok(PositionTagged::new_from_to(
                Statement::If(condition, Box::new(then_branch), None),
                if_pos,
                then_branch_pos,
            )),
        }
    }

    fn block(&mut self) -> LoxResult<PositionTagged<Statement>> {
        let (_, open_pos) = self.match_exact_token(Token::LeftBrace)?;
        let mut statements = Vec::new();

        loop {
            match self.peek().map(|v| v.value()) {
                Some(Token::RightBrace) => {
                    let (_, close_pos) = self.match_exact_token(Token::RightBrace)?;

                    break Ok(PositionTagged::new_from_to(
                        Statement::Block(statements),
                        open_pos,
                        close_pos,
                    ));
                }

                _ => statements.push(self.declaration()?.take().0),
            }
        }
    }

    fn while_statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        let (_, start_pos) = self.match_exact_token(Token::While)?;
        self.match_exact_token(Token::LeftParen)?;

        let (condition, _) = self.expression()?.take();
        self.match_exact_token(Token::RightParen)?;

        let (body, end_pos) = self.statement()?.take();

        Ok(PositionTagged::new_from_to(
            Statement::While(condition, Box::new(body)),
            start_pos,
            end_pos,
        ))
    }

    fn for_statement(&mut self) -> LoxResult<PositionTagged<Statement>> {
        let (_, start_pos) = self.match_exact_token(Token::For)?;
        self.match_exact_token(Token::LeftParen)?;

        let initializer = match self.peek().map(|v| v.value()) {
            Some(Token::Semicolon) => {
                self.match_exact_token(Token::Semicolon)?;
                None
            } // No initializer
            Some(Token::Var) => Some(self.var_declaration()?.take().0),
            _ => Some(self.expression_statement()?.take().0),
        };

        let condition = match self.peek().map(|v| v.value()) {
            Some(Token::Semicolon) => None,
            _ => Some(self.expression()?.take().0),
        };
        self.match_exact_token(Token::Semicolon)?;

        // If there is no condition, we behave as if it is true.
        let condition = condition.unwrap_or_else(|| Expression::Literal(Value::from(true)));

        let increment = match self.peek().map(|v| v.value()) {
            Some(Token::RightParen) => None,
            _ => Some(self.expression()?.take().0),
        };
        self.match_exact_token(Token::RightParen)?;

        let (body, body_pos) = self.statement()?.take();

        // If we have an increment expression, then we need to combine it with the increment
        let body = if let Some(increment) = increment {
            Statement::Block(vec![body, Statement::Expression(increment)])
        } else {
            body
        };

        Ok(PositionTagged::new_from_to(
            if let Some(initializer) = initializer {
                Statement::Block(vec![
                    initializer,
                    Statement::While(condition, Box::new(body)),
                ])
            } else {
                Statement::While(condition, Box::new(body))
            },
            start_pos,
            body_pos,
        ))
    }

    fn expression(&mut self) -> LoxResult<PositionTagged<Expression>> {
        self.comma_sequence()
    }

    fn comma_sequence(&mut self) -> LoxResult<PositionTagged<Expression>> {
        binary_expression! { self.match_binary_expression(assignment) {
            Token::Comma => BinaryOp::Comma,
        } }
    }

    fn assignment(&mut self) -> LoxResult<PositionTagged<Expression>> {
        let (expr, expr_pos) = self.logic_or()?.take();

        match &expr {
            Expression::VariableGet(name) => match self.peek().map(|t| t.value()) {
                Some(Token::Equal) => {
                    self.match_exact_token(Token::Equal)?;
                    let (value, value_pos) = self.assignment()?.take();

                    Ok(PositionTagged::new_from_to(
                        Expression::Assignment(name.to_string(), Box::new(value)),
                        expr_pos,
                        value_pos,
                    ))
                }

                _ => Ok(PositionTagged::new(expr, expr_pos)),
            },

            _ => Ok(PositionTagged::new(expr, expr_pos)),
        }
    }

    fn logic_or(&mut self) -> LoxResult<PositionTagged<Expression>> {
        binary_expression! { self.match_logical_binary_expression(logic_and) {
            Token::Or => LogicalBinaryOp::Or,
        } }
    }

    fn logic_and(&mut self) -> LoxResult<PositionTagged<Expression>> {
        binary_expression! { self.match_logical_binary_expression(equality) {
            Token::And => LogicalBinaryOp::And,
        } }
    }

    fn equality(&mut self) -> LoxResult<PositionTagged<Expression>> {
        binary_expression! { self.match_binary_expression(ternary) {
            Token::EqualEqual => BinaryOp::EqualEqual,
            Token::BangEqual => BinaryOp::BangEqual,
        } }
    }

    fn ternary(&mut self) -> LoxResult<PositionTagged<Expression>> {
        let (comparison_expr, comparison_pos) = self.comparison()?.take();

        if let Some(Token::QuestionMark) = self.peek().map(|t| t.value()) {
            self.advance();

            let (true_expr, _) = self.expression()?.take();

            self.match_exact_token(Token::Colon)?;

            let (false_expr, false_expr_pos) = self.expression()?.take();

            Ok(PositionTagged::new_from_to(
                Expression::Ternary(
                    Box::new(comparison_expr),
                    Box::new(true_expr),
                    Box::new(false_expr),
                ),
                comparison_pos,
                false_expr_pos,
            ))
        } else {
            Ok(PositionTagged::new(comparison_expr, comparison_pos))
        }
    }

    fn comparison(&mut self) -> LoxResult<PositionTagged<Expression>> {
        binary_expression! { self.match_binary_expression(term) {
            Token::Greater => BinaryOp::Greater,
            Token::GreaterEqual => BinaryOp::GreaterEqual,
            Token::Less => BinaryOp::Less,
            Token::LessEqual => BinaryOp::LessEqual,
        } }
    }

    fn term(&mut self) -> LoxResult<PositionTagged<Expression>> {
        binary_expression! { self.match_binary_expression(factor) {
            Token::Plus => BinaryOp::Plus,
            Token::Minus => BinaryOp::Minus,
        } }
    }

    fn factor(&mut self) -> LoxResult<PositionTagged<Expression>> {
        binary_expression! { self.match_binary_expression(unary) {
            Token::Star => BinaryOp::Star,
            Token::Slash => BinaryOp::Slash,
        } }
    }

    fn emit_unary_op(&mut self, operator: UnaryOp) -> LoxResult<PositionTagged<Expression>> {
        let (_, pos) = self.advance().unwrap().take();
        Ok(PositionTagged::new(
            Expression::Unary(operator, Box::new(self.unary()?.take().0)),
            pos,
        ))
    }

    fn unary(&mut self) -> LoxResult<PositionTagged<Expression>> {
        match self.peek().map(|t| t.value()) {
            Some(Token::Minus) => self.emit_unary_op(UnaryOp::Minus),
            Some(Token::Bang) => self.emit_unary_op(UnaryOp::Bang),

            Some(Token::LeftParen) => {
                let (_, start_pos) = self.advance().unwrap().take();
                let expr = self.expression()?.take().0;
                let expr = Box::new(expr);
                let (_, end_pos) = self.match_exact_token(Token::RightParen)?;

                Ok(PositionTagged::new_from_to(
                    Expression::Grouping(expr),
                    start_pos,
                    end_pos,
                ))
            }

            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> LoxResult<PositionTagged<Expression>> {
        match self.advance().map(|t| t.take()) {
            Some((Token::Literal(value), pos)) => {
                Ok(PositionTagged::new(Expression::Literal(value), pos))
            }
            Some((Token::Identifier(name), pos)) => {
                Ok(PositionTagged::new(Expression::VariableGet(name), pos))
            }
            Some((Token::EndOfFile, pos)) => Err(LoxError::IncompleteExpression(pos)),
            Some((token, pos)) => Err(LoxError::UnexpectedToken(token, pos)),
            None => panic!("End of file token missing"),
        }
    }
}

impl<Iter: Iterator<Item = PositionTagged<Token>>> Iterator for Parser<Iter> {
    type Item = LoxResult<PositionTagged<Statement>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek().map(|t| t.value()) {
            Some(Token::EndOfFile) | None => None,
            _ => Some(self.declaration()),
        }
    }
}

pub fn parse(
    tokens: impl IntoIterator<Item = PositionTagged<Token>>,
) -> impl Iterator<Item = LoxResult<PositionTagged<Statement>>> {
    Parser::new(tokens.into_iter())
}
