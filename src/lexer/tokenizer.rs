use crate::{FilePos, LoxError, LoxResult, Position, PositionTagged, tag_position};
use std::{fmt, fs, iter::Peekable, io::{self, Read}, path::Path, str::Chars};
use super::Token;

struct TokenReader<'a, 'b> {
    code: Peekable<Chars<'a>>,
    file_name: &'b str,
    start_pos: FilePos,
    current_pos: FilePos,
}

impl<'a, 'b> TokenReader<'a, 'b> {
    fn new(code: &'a str, file_name: &'b str, line: u32) -> Self {
        Self {
            code: code.chars().peekable(),
            file_name,
            start_pos: FilePos::new(line, 0),
            current_pos: FilePos::new(line, 0),
        }
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
        if self.code.peek().cloned() == Some(c) {
            self.advance();
            true
        } else {
            false
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
        self.start_pos = self.current_pos;

        match self.advance() {
            None => None,
            Some('(') => Some(self.emit_token(Token::LeftParen)),

            Some(c) => Some(Err(LoxError::InvalidCharacter(c, self.token_position())))
        }
    }
}

pub fn tokenize<'a, 'b>(code: &'a str, file_name: &'b str, line: u32) -> LoxResult<'b, Vec<PositionTagged<'b, Token>>> {
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

    fn assert_token_match(result: &PositionTagged<'_, Token>, token: Token, file_name: &str, start_pos: FilePos, end_pos: Option<FilePos>) {
        assert_eq!(*result.value(), token);
        assert_eq!(result.position().file_name(), file_name);
        assert_eq!(*result.position().start(), start_pos);
        assert_eq!(*result.position().end(), end_pos);
    }

    fn assert_error_match(error: &LoxError<'_>, expected: char, file_name: &str, start_pos: FilePos, end_pos: Option<FilePos>) {
        match error {
            LoxError::InvalidCharacter(ch, pos) => {
                assert_eq!(*ch, expected);
                assert_eq!(pos.file_name(), file_name);
                assert_eq!(*pos.start(), start_pos);
                assert_eq!(*pos.end(), end_pos);
            },

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
            Self { token, start_col, end_col }
        }
    }

    struct SimpleErrorMatch {
        ch: char,
        start_col: u32,
        end_col: u32,
    }

    impl SimpleErrorMatch {
        fn new(ch: char, start_col: u32, end_col: u32) -> Self {
            Self { ch, start_col, end_col }
        }
    }

    fn assert_tokens(tokens: &str, expected: &[SimpleTokenMatch]) {
        let tokens = tokenize(tokens, "filename", 0).expect("Failed to match tokens");

        assert_eq!(tokens.len(), expected.len());
        for idx in 0..tokens.len() {
            assert_token_match(&tokens[idx], expected[idx].token, "filename", FilePos::new(0, expected[idx].start_col), Some(FilePos::new(0, expected[idx].end_col)));
        }
    }

    fn assert_errors(tokens: &str, expected: &[SimpleErrorMatch]) {
        if let Err(LoxError::TokenizationError(errors)) = tokenize(tokens, "filename", 0) {
            assert_eq!(errors.len(), expected.len());
            for idx in 0..errors.len() {
                assert_error_match(&errors[idx], expected[idx].ch, "filename", FilePos::new(0, expected[idx].start_col), Some(FilePos::new(0, expected[idx].end_col)));
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
    }
}