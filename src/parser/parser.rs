use super::Expression;
use crate::{LoxError, LoxResult, Position, PositionTagged, Token};
use std::iter::Peekable;

pub struct Parser<Iter: Iterator<Item = PositionTagged<Token>>> {
    tokens: Peekable<Iter>,
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

    fn expression(&mut self) -> LoxResult<PositionTagged<Expression>> {
        self.equality()
    }

    fn equality(&mut self) -> LoxResult<PositionTagged<Expression>> {
        let mut expr = self.comparison()?;
        let start_pos = expr.position().start().clone();

        loop {
            match self.peek().map(|t| t.value()) {
                Some(operator @ (Token::EqualEqual | Token::BangEqual)) => {
                    let (operator, _) = self.advance().unwrap().take();

                    let (left, left_pos) = expr.take();
                    let (right, right_pos) = self.comparison()?.take();

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

        /*let start_pos = expr.pos().start_pos();

        loop {
            match self.peek().map(|t| t.value()) {
                Some(operator & (Token::EqualEqual | Token::BangEqual)) => {
                    self.advance();

                    let right = self.comparison();
                    expr = Expression::BinaryExpression(expr, operator, right);
                }
            }
        }*/
    }

    fn comparison(&mut self) -> LoxResult<PositionTagged<Expression>> {
        self.unary()
    }

    fn unary(&mut self) -> LoxResult<PositionTagged<Expression>> {
        match self.peek().map(|t| t.value()) {
            Some(Token::Minus | Token::Bang) => {
                let (operator, pos) = self.advance().unwrap().take();
                Ok(PositionTagged::new(
                    Expression::Unary(operator, Box::new(self.unary()?.take().0)),
                    pos,
                ))
            }

            Some(Token::LeftParen) => {
                let (_, start_pos) = self.advance().unwrap().take();
                let expr = self.expression()?.take().0;
                let expr = Box::new(expr);
                match self.advance().map(|t| t.take()) {
                    Some((Token::RightParen, end_pos)) => Ok(PositionTagged::new_from_to(
                        Expression::Grouping(expr),
                        start_pos,
                        end_pos,
                    )),

                    Some((Token::EndOfFile, pos)) => Err(LoxError::IncompleteExpression(pos)),
                    Some((token, pos)) => Err(LoxError::UnexpectedToken(token, pos)),
                    None => panic!("End of file token missing"),
                }
            }

            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> LoxResult<PositionTagged<Expression>> {
        match self.advance().map(|t| t.take()) {
            Some((
                literal
                @
                (Token::Nil
                | Token::True
                | Token::False
                | Token::Number(_)
                | Token::String(_)),
                pos,
            )) => Ok(PositionTagged::new(Expression::Literal(literal), pos)),

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
        test_parse(vec![Token::Number(1.0), Token::EndOfFile]);
    }
}
