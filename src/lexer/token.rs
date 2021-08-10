use crate::Value;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
#[repr(u8)]
pub enum SimpleToken {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Slash,
    QuestionMark,
    Colon,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    EndOfFile,
}

impl fmt::Display for SimpleToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(match self {
            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::LeftBrace => "{{",
            Self::RightBrace => "}}",
            Self::Comma => ",",
            Self::Dot => ".",
            Self::Minus => "-",
            Self::Plus => "+",
            Self::Semicolon => ";",
            Self::Star => "*",
            Self::Bang => "!",
            Self::BangEqual => "!=",
            Self::Equal => "=",
            Self::EqualEqual => "==",
            Self::Less => "<",
            Self::LessEqual => "<=",
            Self::Greater => ">",
            Self::GreaterEqual => ">=",
            Self::Slash => "/",
            Self::QuestionMark => "?",
            Self::Colon => ":",
            Self::And => "and",
            Self::Class => "class",
            Self::Else => "else",
            Self::False => "false",
            Self::For => "for",
            Self::Fun => "fun",
            Self::If => "if",
            Self::Nil => "nil",
            Self::Or => "or",
            Self::Print => "print",
            Self::Return => "return",
            Self::Super => "super",
            Self::This => "this",
            Self::True => "true",
            Self::Var => "var",
            Self::While => "while",
            Self::EndOfFile => "<EOF>",
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Simple(SimpleToken),
    Literal(Value),
    Identifier(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Simple(simple_token) => simple_token.fmt(f),
            Self::Literal(val) => val.fmt(f),
            Self::Identifier(id) => id.fmt(f),
        }
    }
}
