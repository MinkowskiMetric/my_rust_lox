use super::Token;
use crate::{FilePos, LoxError, LoxResult, Position, PositionTagged};
use std::{fs, io::Read, iter::Peekable, str::Chars};

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

struct TokenReader<'a, 'b> {
    code: CharPeeper<'a>,
    file_name: &'b str,
    start_pos: FilePos,
    current_pos: FilePos,
}

impl<'a, 'b> TokenReader<'a, 'b> {
    fn new(code: &'a str, file_name: &'b str, line: u32) -> Self {
        Self {
            code: CharPeeper::new(code),
            file_name,
            start_pos: FilePos::new(line, 0),
            current_pos: FilePos::new(line, 0),
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

    fn emit_token(&self, token: Token) -> LoxResult<'b, PositionTagged<'b, Token>> {
        Ok(PositionTagged::new(token, self.token_position()))
    }

    fn token_position(&self) -> Position<'b> {
        Position::new(self.file_name, self.start_pos, Some(self.current_pos))
    }
}

impl<'a, 'b> Iterator for TokenReader<'a, 'b> {
    type Item = LoxResult<'b, PositionTagged<'b, Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        self.start_pos = self.current_pos;

        match self.advance() {
            None => None,

            // Single characters are simple
            Some('(') => Some(self.emit_token(Token::LeftParen)),
            Some(')') => Some(self.emit_token(Token::RightParen)),
            Some('{') => Some(self.emit_token(Token::LeftBrace)),
            Some('}') => Some(self.emit_token(Token::RightBrace)),
            Some(',') => Some(self.emit_token(Token::Comma)),
            Some('.') => Some(self.emit_token(Token::Dot)),
            Some('-') => Some(self.emit_token(Token::Minus)),
            Some('+') => Some(self.emit_token(Token::Plus)),
            Some(';') => Some(self.emit_token(Token::Semicolon)),
            Some('*') => Some(self.emit_token(Token::Star)),
            Some('/') => Some(self.emit_token(Token::Slash)),

            // One or two character tokens
            Some('!') if self.match_advance('=') => Some(self.emit_token(Token::BangEqual)),
            Some('!') => Some(self.emit_token(Token::Bang)),
            Some('=') if self.match_advance('=') => Some(self.emit_token(Token::EqualEqual)),
            Some('=') => Some(self.emit_token(Token::Equal)),
            Some('<') if self.match_advance('=') => Some(self.emit_token(Token::LessEqual)),
            Some('<') => Some(self.emit_token(Token::Less)),
            Some('>') if self.match_advance('=') => Some(self.emit_token(Token::GreaterEqual)),
            Some('>') => Some(self.emit_token(Token::Greater)),

            Some(c) => Some(Err(LoxError::InvalidCharacter(c, self.token_position()))),
        }
    }
}

pub fn tokenize<'a, 'b>(
    code: &'a str,
    file_name: &'b str,
    line: u32,
) -> LoxResult<'b, Vec<PositionTagged<'b, Token>>> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    for result in TokenReader::new(code, file_name, line) {
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

pub fn tokenize_file<'a>(path: &'a str) -> LoxResult<'a, Vec<PositionTagged<'a, Token>>> {
    let mut file = fs::File::open(path)?;

    let mut code = String::new();
    file.read_to_string(&mut code)?;

    tokenize(&code, path, 1)
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_token_match(
        result: &PositionTagged<'_, Token>,
        token: Token,
        file_name: &str,
        start_pos: FilePos,
        end_pos: Option<FilePos>,
    ) {
        assert_eq!(*result.value(), token);
        assert_eq!(result.position().file_name(), file_name);
        assert_eq!(*result.position().start(), start_pos);
        assert_eq!(*result.position().end(), end_pos);
    }

    fn assert_error_match(
        error: &LoxError<'_>,
        expected: char,
        file_name: &str,
        start_pos: FilePos,
        end_pos: Option<FilePos>,
    ) {
        match error {
            LoxError::InvalidCharacter(ch, pos) => {
                assert_eq!(*ch, expected);
                assert_eq!(pos.file_name(), file_name);
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

        assert_eq!(tokens.len(), expected.len());
        for idx in 0..tokens.len() {
            assert_token_match(
                &tokens[idx],
                expected[idx].token,
                "filename",
                FilePos::new(0, expected[idx].start_col),
                Some(FilePos::new(0, expected[idx].end_col)),
            );
        }
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
                    Some(FilePos::new(0, expected[idx].end_col)),
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
        assert_single_token("(", Token::LeftParen, 0, 1);
        assert_single_error("#", '#', 0, 1);

        assert_tokens(
            "\t( )!=!!>>=<<====/",
            &[
                SimpleTokenMatch::new(Token::LeftParen, 1, 2),
                SimpleTokenMatch::new(Token::RightParen, 3, 4),
                SimpleTokenMatch::new(Token::BangEqual, 4, 6),
                SimpleTokenMatch::new(Token::Bang, 6, 7),
                SimpleTokenMatch::new(Token::Bang, 7, 8),
                SimpleTokenMatch::new(Token::Greater, 8, 9),
                SimpleTokenMatch::new(Token::GreaterEqual, 9, 11),
                SimpleTokenMatch::new(Token::Less, 11, 12),
                SimpleTokenMatch::new(Token::LessEqual, 12, 14),
                SimpleTokenMatch::new(Token::EqualEqual, 14, 16),
                SimpleTokenMatch::new(Token::Equal, 16, 17),
                SimpleTokenMatch::new(Token::Slash, 17, 18),
            ],
        );

        assert_token_match(
            &tokenize("\n\n(", "boo", 0).expect("failed to parse")[0],
            Token::LeftParen,
            "boo",
            FilePos::new(2, 0),
            Some(FilePos::new(2, 1)),
        );
        assert_token_match(
            &tokenize("\n  // hello world\n(\n// not to end of line", "boo", 0)
                .expect("failed to parse")[0],
            Token::LeftParen,
            "boo",
            FilePos::new(2, 0),
            Some(FilePos::new(2, 1)),
        );
    }
}
