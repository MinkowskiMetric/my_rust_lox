use super::{SimpleToken, Token};
use crate::{FilePos, LoxError, LoxResult, Position, PositionedToken};
use lazy_static::lazy_static;
use maplit::hashmap;
use std::{
    collections::HashMap,
    fs,
    io::Read,
    rc::Rc,
    str::{Chars, FromStr},
};

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, SimpleToken> = hashmap! {
        "and" => SimpleToken::And,
        "class" => SimpleToken::Class,
        "else" => SimpleToken::Else,
        "false" => SimpleToken::False,
        "for" => SimpleToken::For,
        "fun" => SimpleToken::Fun,
        "if" => SimpleToken::If,
        "nil" => SimpleToken::Nil,
        "or" => SimpleToken::Or,
        "print" => SimpleToken::Print,
        "return" => SimpleToken::Return,
        "super" => SimpleToken::Super,
        "this" => SimpleToken::This,
        "true" => SimpleToken::True,
        "var" => SimpleToken::Var,
        "while" => SimpleToken::While,
    };
}

struct CharPeeper<'a> {
    chars: Chars<'a>,
    peek1: Option<char>,
    peek2: Option<char>,
}

impl<'a> CharPeeper<'a> {
    fn new(code: &'a str) -> Self {
        let mut chars = code.chars();
        let peek1 = chars.next();
        let peek2 = chars.next();

        Self {
            chars,
            peek1,
            peek2,
        }
    }

    fn next(&mut self) -> Option<char> {
        if let Some(c) = self.peek1.take() {
            self.peek1 = self.peek2;
            self.peek2 = self.chars.next();
            Some(c)
        } else {
            None
        }
    }

    fn peek1(&mut self) -> Option<char> {
        self.peek1
    }

    fn peek2(&mut self) -> Option<char> {
        self.peek2
    }
}

struct TokenReader<'a> {
    code: CharPeeper<'a>,
    file_name: Rc<String>,
    start_pos: FilePos,
    current_pos: FilePos,
    file_ended: bool,
}

impl<'a> TokenReader<'a> {
    fn new(code: &'a str, file_name: Rc<String>, line: u32) -> Self {
        Self {
            code: CharPeeper::new(code),
            file_name,
            start_pos: FilePos::new(line, 0),
            current_pos: FilePos::new(line, 0),
            file_ended: false,
        }
    }

    fn peek1(&mut self) -> Option<char> {
        self.code.peek1()
    }

    fn peek2(&mut self) -> Option<char> {
        self.code.peek2()
    }

    fn advance(&mut self) -> Option<char> {
        match self.code.next() {
            None => None,
            Some(c) => {
                self.current_pos.advance_col();
                Some(c)
            }
        }
    }

    fn match_advance(&mut self, c: char) -> bool {
        if self.peek1() == Some(c) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek1() {
                Some(' ' | '\r' | '\t') => {
                    self.advance();
                }
                Some('\n') => {
                    self.advance();
                    self.current_pos.new_line();
                }
                Some('/') if self.peek2() == Some('/') => self.advance_to_end_of_line(),
                _ => break,
            };
        }
    }

    fn advance_to_end_of_line(&mut self) {
        loop {
            match self.advance() {
                Some('\n') => {
                    self.current_pos.new_line();
                    break;
                }
                None => break,
                _ => (),
            }
        }
    }

    fn consume_string(&mut self) -> LoxResult<PositionedToken> {
        let mut ret = String::new();

        loop {
            match self.advance() {
                None => return Err(LoxError::UnexpectedEndOfFile(self.token_position())),
                Some('"') => return self.emit_token(Token::Literal(ret.into())),

                Some('\\') => match self.advance() {
                    None => return Err(LoxError::UnexpectedEndOfFile(self.token_position())),
                    Some('\\') => ret.push('\\'),
                    Some('n') => ret.push('\n'),
                    Some('t') => ret.push('\t'),
                    Some('r') => ret.push('\r'),
                    Some('0') => ret.push('\0'),
                    Some('"') => ret.push('"'),
                    Some(c) => {
                        return Err(LoxError::UnknownEscapeSequence(c, self.token_position()))
                    }
                },

                Some('\n') => {
                    self.current_pos.new_line();
                    ret.push('\n');
                }
                Some(c) => ret.push(c),
            }
        }
    }

    fn consume_identifier(&mut self, first_char: char) -> LoxResult<PositionedToken> {
        let mut ret: String = first_char.into();

        loop {
            match self.peek1() {
                Some(next_char @ ('_' | 'a'..='z' | 'A'..='Z' | '0'..='9')) => {
                    self.advance();
                    ret.push(next_char)
                }

                _ => return self.emit_identifier(ret),
            }
        }
    }

    fn consume_number(&mut self, first_digit: char) -> LoxResult<PositionedToken> {
        let mut num_str: String = first_digit.into();

        loop {
            match self.peek1() {
                Some(next_digit @ ('.' | '0'..='9')) => {
                    self.advance();
                    num_str.push(next_digit);
                }

                _ => {
                    return match f64::from_str(&num_str) {
                        Ok(num) => self.emit_token(Token::Literal(num.into())),
                        Err(e) => Err(LoxError::InvalidNumber(e, self.token_position())),
                    };
                }
            }
        }
    }

    fn emit_identifier(&self, identifier: String) -> LoxResult<PositionedToken> {
        match KEYWORDS.get(&*identifier).cloned() {
            Some(token) => self.emit_token(Token::Simple(token)),
            None => self.emit_token(Token::Identifier(identifier)),
        }
    }

    fn emit_token(&self, token: Token) -> LoxResult<PositionedToken> {
        Ok(PositionedToken::new(token, self.token_position()))
    }

    fn token_position(&self) -> Position {
        Position::new(self.file_name.clone(), self.start_pos, self.current_pos)
    }
}

impl<'a> Iterator for TokenReader<'a> {
    type Item = LoxResult<PositionedToken>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        self.start_pos = self.current_pos;

        match self.advance() {
            None => {
                if !self.file_ended {
                    self.file_ended = true;
                    Some(self.emit_token(Token::Simple(SimpleToken::EndOfFile)))
                } else {
                    None
                }
            }

            // Single characters are simple
            Some('(') => Some(self.emit_token(Token::Simple(SimpleToken::LeftParen))),
            Some(')') => Some(self.emit_token(Token::Simple(SimpleToken::RightParen))),
            Some('{') => Some(self.emit_token(Token::Simple(SimpleToken::LeftBrace))),
            Some('}') => Some(self.emit_token(Token::Simple(SimpleToken::RightBrace))),
            Some(',') => Some(self.emit_token(Token::Simple(SimpleToken::Comma))),
            Some('.') => Some(self.emit_token(Token::Simple(SimpleToken::Dot))),
            Some('-') => Some(self.emit_token(Token::Simple(SimpleToken::Minus))),
            Some('+') => Some(self.emit_token(Token::Simple(SimpleToken::Plus))),
            Some(';') => Some(self.emit_token(Token::Simple(SimpleToken::Semicolon))),
            Some('*') => Some(self.emit_token(Token::Simple(SimpleToken::Star))),
            Some('/') => Some(self.emit_token(Token::Simple(SimpleToken::Slash))),
            Some('?') => Some(self.emit_token(Token::Simple(SimpleToken::QuestionMark))),
            Some(':') => Some(self.emit_token(Token::Simple(SimpleToken::Colon))),

            // One or two character tokens
            Some('!') if self.match_advance('=') => {
                Some(self.emit_token(Token::Simple(SimpleToken::BangEqual)))
            }
            Some('!') => Some(self.emit_token(Token::Simple(SimpleToken::Bang))),
            Some('=') if self.match_advance('=') => {
                Some(self.emit_token(Token::Simple(SimpleToken::EqualEqual)))
            }
            Some('=') => Some(self.emit_token(Token::Simple(SimpleToken::Equal))),
            Some('<') if self.match_advance('=') => {
                Some(self.emit_token(Token::Simple(SimpleToken::LessEqual)))
            }
            Some('<') => Some(self.emit_token(Token::Simple(SimpleToken::Less))),
            Some('>') if self.match_advance('=') => {
                Some(self.emit_token(Token::Simple(SimpleToken::GreaterEqual)))
            }
            Some('>') => Some(self.emit_token(Token::Simple(SimpleToken::Greater))),

            Some('"') => Some(self.consume_string()),

            Some(first_char @ ('_' | 'a'..='z' | 'A'..='Z')) => {
                Some(self.consume_identifier(first_char))
            }
            Some(first_digit @ '0'..='9') => Some(self.consume_number(first_digit)),

            Some(c) => Some(Err(LoxError::InvalidCharacter(c, self.token_position()))),
        }
    }
}

pub fn tokenize<'a, 'b>(
    code: &'a str,
    file_name: &'b str,
    line: u32,
) -> LoxResult<Vec<PositionedToken>> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    for result in TokenReader::new(code, Rc::new(file_name.into()), line) {
        match result {
            Ok(token) => tokens.push(token),
            Err(err) => errors.push(err),
        }
    }

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(LoxError::TokenizationError(errors))
    }
}

pub fn tokenize_file(path: &str) -> LoxResult<Vec<PositionedToken>> {
    let mut file = fs::File::open(path)?;

    let mut code = String::new();
    file.read_to_string(&mut code)?;

    tokenize(&code, path, 1)
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_token_match(
        result: &PositionedToken,
        token: &Token,
        file_name: &str,
        start_pos: FilePos,
        end_pos: FilePos,
    ) {
        assert_eq!(*result.token(), *token);
        assert_eq!(result.position().file_name().as_ref(), file_name);
        assert_eq!(*result.position().start(), start_pos);
        assert_eq!(*result.position().end(), end_pos);
    }

    fn assert_error_match(
        error: &LoxError,
        expected: char,
        file_name: &str,
        start_pos: FilePos,
        end_pos: FilePos,
    ) {
        match error {
            LoxError::InvalidCharacter(ch, pos) => {
                assert_eq!(*ch, expected);
                assert_eq!(pos.file_name().as_ref(), file_name);
                assert_eq!(*pos.start(), start_pos);
                assert_eq!(*pos.end(), end_pos);
            }

            _ => panic!("Unexpected LoxError {:?}", error),
        }
    }

    struct SimpleTokenMatch {
        token: Token,
        start_col: u32,
        end_col: u32,
    }

    impl SimpleTokenMatch {
        fn new(token: Token, start_col: u32, end_col: u32) -> Self {
            Self {
                token,
                start_col,
                end_col,
            }
        }
    }

    struct SimpleErrorMatch {
        ch: char,
        start_col: u32,
        end_col: u32,
    }

    impl SimpleErrorMatch {
        fn new(ch: char, start_col: u32, end_col: u32) -> Self {
            Self {
                ch,
                start_col,
                end_col,
            }
        }
    }

    fn assert_tokens(tokens: &str, expected: &[SimpleTokenMatch]) {
        let tokens = tokenize(tokens, "filename", 0).expect("Failed to match tokens");

        assert_eq!(tokens.len(), expected.len() + 1);
        for idx in 0..expected.len() {
            assert_token_match(
                &tokens[idx],
                &expected[idx].token,
                "filename",
                FilePos::new(0, expected[idx].start_col),
                FilePos::new(0, expected[idx].end_col),
            );
        }

        assert_eq!(
            *tokens[expected.len()].token(),
            Token::Simple(SimpleToken::EndOfFile)
        );
    }

    fn assert_errors(tokens: &str, expected: &[SimpleErrorMatch]) {
        if let Err(LoxError::TokenizationError(errors)) = tokenize(tokens, "filename", 0) {
            assert_eq!(errors.len(), expected.len());
            for idx in 0..errors.len() {
                assert_error_match(
                    &errors[idx],
                    expected[idx].ch,
                    "filename",
                    FilePos::new(0, expected[idx].start_col),
                    FilePos::new(0, expected[idx].end_col),
                );
            }
        }
    }

    fn assert_single_token(tokens: &str, token: Token, start_col: u32, end_col: u32) {
        assert_tokens(tokens, &[SimpleTokenMatch::new(token, start_col, end_col)])
    }

    fn assert_single_error(tokens: &str, ch: char, start_col: u32, end_col: u32) {
        assert_errors(tokens, &[SimpleErrorMatch::new(ch, start_col, end_col)])
    }

    #[test]
    fn test_single_token() {
        assert_single_token("(", Token::Simple(SimpleToken::LeftParen), 0, 1);
        assert_single_error("#", '#', 0, 1);

        assert_tokens(
            "\t( )!=!!>>=<<====/\"hello, world\"chimp012+",
            &[
                SimpleTokenMatch::new(Token::Simple(SimpleToken::LeftParen), 1, 2),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::RightParen), 3, 4),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::BangEqual), 4, 6),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::Bang), 6, 7),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::Bang), 7, 8),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::Greater), 8, 9),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::GreaterEqual), 9, 11),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::Less), 11, 12),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::LessEqual), 12, 14),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::EqualEqual), 14, 16),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::Equal), 16, 17),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::Slash), 17, 18),
                SimpleTokenMatch::new(Token::Literal("hello, world".to_string().into()), 18, 32),
                SimpleTokenMatch::new(Token::Identifier("chimp012".to_string().into()), 32, 40),
                SimpleTokenMatch::new(Token::Simple(SimpleToken::Plus), 40, 41),
            ],
        );

        assert_single_token("0", Token::Literal(0.0.into()), 0, 1);
        assert_single_token("0.", Token::Literal(0.0.into()), 0, 2);
        assert_single_token("0.1234", Token::Literal(0.1234.into()), 0, 6);
        assert_single_token("12.34", Token::Literal(12.34.into()), 0, 5);

        assert_token_match(
            &tokenize("\n\n\"one\n\ntwo\"", "boo", 0).expect("failed to parse")[0],
            &Token::Literal("one\n\ntwo".to_string().into()),
            "boo",
            FilePos::new(2, 0),
            FilePos::new(4, 4),
        );
        assert_token_match(
            &tokenize("\n\n(", "boo", 0).expect("failed to parse")[0],
            &Token::Simple(SimpleToken::LeftParen),
            "boo",
            FilePos::new(2, 0),
            FilePos::new(2, 1),
        );
        assert_token_match(
            &tokenize("\n  // hello world\n(\n// not to end of line", "boo", 0)
                .expect("failed to parse")[0],
            &Token::Simple(SimpleToken::LeftParen),
            "boo",
            FilePos::new(2, 0),
            FilePos::new(2, 1),
        );
    }
}
