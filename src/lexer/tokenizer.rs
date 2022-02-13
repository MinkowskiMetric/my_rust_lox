use super::{SimpleToken, Token};
use crate::{CollectedErrors, FilePos, LoxError, Position, PositionedToken};
use lazy_static::lazy_static;
use maplit::hashmap;
use owned_chars::OwnedCharsExt;
use std::{
    collections::HashMap,
    fs,
    io::{self, Read},
    rc::Rc,
    str::FromStr,
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

struct CharPeeper<Iter: Iterator<Item = char>> {
    iter: Iter,
    peek1: Option<char>,
    peek2: Option<char>,
}

impl<Iter: Iterator<Item = char>> CharPeeper<Iter> {
    fn new<Into: IntoIterator<IntoIter = Iter>>(code: Into) -> Self {
        let mut iter = code.into_iter();
        let peek1 = iter.next();
        let peek2 = iter.next();

        Self { iter, peek1, peek2 }
    }

    fn next(&mut self) -> Option<char> {
        if let Some(c) = self.peek1.take() {
            self.peek1 = self.peek2;
            self.peek2 = self.iter.next();
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

pub struct TokenReader<Iter: Iterator<Item = char>> {
    code: Option<CharPeeper<Iter>>,
    file_name: Rc<String>,
    start_pos: FilePos,
    current_pos: FilePos,
    errors: Option<Vec<LoxError>>,
}

impl<Iter: Iterator<Item = char>> TokenReader<Iter> {
    fn new<Into: IntoIterator<IntoIter = Iter>>(
        code: Into,
        file_name: Rc<String>,
        line: u32,
    ) -> Self {
        Self {
            code: Some(CharPeeper::new(code)),
            file_name,
            start_pos: FilePos::new(line, 0),
            current_pos: FilePos::new(line, 0),
            errors: None,
        }
    }

    fn peek1(&mut self) -> Option<char> {
        self.code.as_mut().and_then(CharPeeper::peek1)
    }

    fn peek2(&mut self) -> Option<char> {
        self.code.as_mut().and_then(CharPeeper::peek2)
    }

    fn advance(&mut self) -> Option<char> {
        match self.code.as_mut().and_then(CharPeeper::next) {
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

    fn consume_string(&mut self) -> Option<PositionedToken> {
        let mut ret = String::new();

        loop {
            match self.advance() {
                None => {
                    return self.emit_error(LoxError::UnexpectedEndOfFile(self.token_position()))
                }
                Some('"') => {
                    return self.emit_token(Token::Literal(ret.into()));
                }

                Some('\\') => match self.advance() {
                    None => {
                        return self
                            .emit_error(LoxError::UnexpectedEndOfFile(self.token_position()))
                    }
                    Some('\\') => ret.push('\\'),
                    Some('n') => ret.push('\n'),
                    Some('t') => ret.push('\t'),
                    Some('r') => ret.push('\r'),
                    Some('0') => ret.push('\0'),
                    Some('"') => ret.push('"'),
                    Some(c) => {
                        return self
                            .emit_error(LoxError::UnknownEscapeSequence(c, self.token_position()))
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

    fn consume_identifier(&mut self, first_char: char) -> Option<PositionedToken> {
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

    fn consume_number(&mut self, first_digit: char) -> Option<PositionedToken> {
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
                        Err(e) => {
                            self.emit_error(LoxError::InvalidNumber(e, self.token_position()))
                        }
                    };
                }
            }
        }
    }

    fn emit_identifier(&self, identifier: String) -> Option<PositionedToken> {
        match KEYWORDS.get(&*identifier).cloned() {
            Some(token) => self.emit_simple_token(token),
            None => self.emit_token(Token::Identifier(identifier)),
        }
    }

    fn emit_simple_token(&self, simple_token: SimpleToken) -> Option<PositionedToken> {
        self.emit_token(Token::Simple(simple_token))
    }

    fn emit_token(&self, token: Token) -> Option<PositionedToken> {
        Some(PositionedToken::new(token, self.token_position()))
    }

    fn token_position(&self) -> Position {
        Position::new(self.file_name.clone(), self.start_pos, self.current_pos)
    }

    fn emit_error(&mut self, error: LoxError) -> Option<PositionedToken> {
        self.errors.get_or_insert_with(Vec::new).push(error);
        None
    }
}

impl<Iter: Iterator<Item = char>> Iterator for TokenReader<Iter> {
    type Item = PositionedToken;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            self.skip_whitespace();
            self.start_pos = self.current_pos;

            let token = match self.advance() {
                None => match self.code {
                    Some(_) => {
                        self.code = None;
                        self.emit_simple_token(SimpleToken::EndOfFile)
                    }
                    None => return None,
                },

                // Single characters are simple
                Some('(') => self.emit_simple_token(SimpleToken::LeftParen),
                Some(')') => self.emit_simple_token(SimpleToken::RightParen),
                Some('{') => self.emit_simple_token(SimpleToken::LeftBrace),
                Some('}') => self.emit_simple_token(SimpleToken::RightBrace),
                Some(',') => self.emit_simple_token(SimpleToken::Comma),
                Some('.') => self.emit_simple_token(SimpleToken::Dot),
                Some('-') => self.emit_simple_token(SimpleToken::Minus),
                Some('+') => self.emit_simple_token(SimpleToken::Plus),
                Some(';') => self.emit_simple_token(SimpleToken::Semicolon),
                Some('*') => self.emit_simple_token(SimpleToken::Star),
                Some('/') => self.emit_simple_token(SimpleToken::Slash),
                Some('?') => self.emit_simple_token(SimpleToken::QuestionMark),
                Some(':') => self.emit_simple_token(SimpleToken::Colon),

                // One or two character tokens
                Some('!') if self.match_advance('=') => {
                    self.emit_simple_token(SimpleToken::BangEqual)
                }
                Some('!') => self.emit_simple_token(SimpleToken::Bang),
                Some('=') if self.match_advance('=') => {
                    self.emit_simple_token(SimpleToken::EqualEqual)
                }
                Some('=') => self.emit_simple_token(SimpleToken::Equal),
                Some('<') if self.match_advance('=') => {
                    self.emit_simple_token(SimpleToken::LessEqual)
                }
                Some('<') => self.emit_simple_token(SimpleToken::Less),
                Some('>') if self.match_advance('=') => {
                    self.emit_simple_token(SimpleToken::GreaterEqual)
                }
                Some('>') => self.emit_simple_token(SimpleToken::Greater),

                Some('"') => self.consume_string(),

                Some(first_char @ ('_' | 'a'..='z' | 'A'..='Z')) => {
                    self.consume_identifier(first_char)
                }
                Some(first_digit @ '0'..='9') => self.consume_number(first_digit),

                _ => None,
            };

            if let Some(token) = token {
                break Some(token);
            }
        }
    }
}

impl<Iter: Iterator<Item = char>> CollectedErrors for TokenReader<Iter> {
    fn errors(self) -> Option<Vec<LoxError>> {
        self.errors
    }
}

pub fn tokenize<'a, 'b>(
    code: &'a str,
    file_name: &'b str,
    line: u32,
) -> impl CollectedErrors<Item = PositionedToken> + 'a {
    TokenReader::new(code.chars(), Rc::new(file_name.into()), line)
}

pub fn tokenize_file(
    path: &str,
) -> Result<impl CollectedErrors<Item = PositionedToken>, io::Error> {
    let mut file = fs::File::open(path)?;

    let mut code = String::new();
    file.read_to_string(&mut code)?;

    Ok(TokenReader::new(code.into_chars(), Rc::new(path.into()), 1))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::CompilerResult;

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
        let tokens: Vec<_> = tokenize(tokens, "filename", 0)
            .result()
            .expect("Failed to match tokens");

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
        if let CompilerResult::Failed(errors) = tokenize(tokens, "filename", 0).result::<Vec<_>>() {
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
            &tokenize("\n\n\"one\n\ntwo\"", "boo", 0)
                .result::<Vec<_>>()
                .expect("failed to parse")[0],
            &Token::Literal("one\n\ntwo".to_string().into()),
            "boo",
            FilePos::new(2, 0),
            FilePos::new(4, 4),
        );
        assert_token_match(
            &tokenize("\n\n(", "boo", 0)
                .result::<Vec<_>>()
                .expect("failed to parse")[0],
            &Token::Simple(SimpleToken::LeftParen),
            "boo",
            FilePos::new(2, 0),
            FilePos::new(2, 1),
        );
        assert_token_match(
            &tokenize("\n  // hello world\n(\n// not to end of line", "boo", 0)
                .result::<Vec<_>>()
                .expect("failed to parse")[0],
            &Token::Simple(SimpleToken::LeftParen),
            "boo",
            FilePos::new(2, 0),
            FilePos::new(2, 1),
        );
    }
}
