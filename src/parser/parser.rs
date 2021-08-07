use super::Expression;
use crate::{BinaryOp, LoxError, LoxResult, Position, PositionTagged, Token, UnaryOp};
use std::iter::Peekable;

pub struct Parser<Iter: Iterator<Item = PositionTagged<Token>>> {
    tokens: Peekable<Iter>,
}

macro_rules! binary_expression {
    ($prsr:ident.match_binary_expression($next:ident) { $(Token::$token:ident => BinaryOp::$op:ident,)+ }) => {
        let mut left = $prsr.$next()?;

        loop {
            match $prsr.peek().map(|t| t.value()) {
                $(Some(Token::$token) => {
                    // Skip past the operator since we already know what it is
                    $prsr.advance();

                    let (left_expr, left_expr_pos) = left.take();
                    let (right_expr, right_expr_pos) = $prsr.$next()?.take();

                    left = PositionTagged::new(
                        Expression::Binary(Box::new(left_expr), BinaryOp::$op, Box::new(right_expr)),
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
    /*($prsr:ident, $next:ident, Token::$first_op:ident $(| Token::$ops:ident)*) => {
           let mut expr = $prsr.$next()?;

           loop {
               match $prsr.peek().map(|t| t.value()) {
                   Some(Token::$first_op $(| Token::$ops)*) => {
                       let (operator, _) = $prsr.advance().unwrap().take();

                       let (left, left_pos) = expr.take();
                       let (right, right_pos) = $prsr.$next()?.take();

                       expr = PositionTagged::new(
                           Expression::Binary(Box::new(left), operator.clone(), Box::new(right)),
                           Position::new(
                               left_pos.file_name().clone(),
                               *left_pos.start(),
                               *right_pos.end(),
                           ),
                       );
                   }

                   _ => break,
               }
           }

           Ok(expr)
       };*/
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

    fn match_exact_token(&mut self, token: Token) -> LoxResult<(Token, Position)> {
        match self.advance().map(|t| t.take()) {
            Some((check_token, pos)) if check_token == token => Ok((check_token, pos)),

            Some((Token::EndOfFile, pos)) => Err(LoxError::IncompleteExpression(pos)),
            Some((check_token, pos)) => Err(LoxError::UnexpectedToken(check_token, pos)),
            None => panic!("End of file token missing"),
        }
    }

    fn expression(&mut self) -> LoxResult<PositionTagged<Expression>> {
        self.commma_sequence()
    }

    fn commma_sequence(&mut self) -> LoxResult<PositionTagged<Expression>> {
        binary_expression! { self.match_binary_expression(equality) {
            Token::Comma => BinaryOp::Comma,
        } }
    }

    fn equality(&mut self) -> LoxResult<PositionTagged<Expression>> {
        binary_expression! { self.match_binary_expression(ternary) {
            Token::EqualEqual => BinaryOp::EqualEqual,
            Token::BangEqual => BinaryOp::BangEqual,
        } }
    }

    fn ternary(&mut self) -> LoxResult<PositionTagged<Expression>> {
        let (mut comparison_expr, comparison_pos) = self.comparison()?.take();

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
            Some((Token::EndOfFile, pos)) => Err(LoxError::IncompleteExpression(pos)),
            Some((token, pos)) => Err(LoxError::UnexpectedToken(token, pos)),
            None => panic!("End of file token missing"),
        }
    }
}

pub fn parse(tokens: Vec<PositionTagged<Token>>) -> LoxResult<PositionTagged<Expression>> {
    let mut parser = Parser::new(tokens.into_iter());

    // This seems like a nice place to start
    parser.expression()
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_parse(tokens: Vec<Token>) {
        let tokens: Vec<_> = tokens.into_iter().map(|a| a.into()).collect();
        parse(tokens).expect("Failed to parse");
    }

    #[test]
    fn test_parser() {
        test_parse(vec![Token::Literal(1.0.into()), Token::EndOfFile]);
    }
}
