use ordered_float::NotNan;

use crate::{LoxToken, LoxTokenType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OwnedLoxTokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Questionmark, // for ?:
    Colon,        // for ?:

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String(String),
    Number(NotNan<f64>),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
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

    // EOF.
    EOF,
}

impl<'a> From<&LoxTokenType<'a>> for OwnedLoxTokenType {
    fn from(token: &LoxTokenType<'a>) -> Self {
        match token {
            LoxTokenType::LeftParen => OwnedLoxTokenType::LeftParen,
            LoxTokenType::RightParen => OwnedLoxTokenType::RightParen,
            LoxTokenType::LeftBrace => OwnedLoxTokenType::LeftBrace,
            LoxTokenType::RightBrace => OwnedLoxTokenType::RightBrace,
            LoxTokenType::Comma => OwnedLoxTokenType::Comma,
            LoxTokenType::Dot => OwnedLoxTokenType::Dot,
            LoxTokenType::Minus => OwnedLoxTokenType::Minus,
            LoxTokenType::Plus => OwnedLoxTokenType::Plus,
            LoxTokenType::Semicolon => OwnedLoxTokenType::Semicolon,
            LoxTokenType::Slash => OwnedLoxTokenType::Slash,
            LoxTokenType::Star => OwnedLoxTokenType::Star,
            LoxTokenType::Questionmark => OwnedLoxTokenType::Questionmark,
            LoxTokenType::Colon => OwnedLoxTokenType::Colon,
            LoxTokenType::Bang => OwnedLoxTokenType::Bang,
            LoxTokenType::BangEqual => OwnedLoxTokenType::BangEqual,
            LoxTokenType::Equal => OwnedLoxTokenType::Equal,
            LoxTokenType::EqualEqual => OwnedLoxTokenType::EqualEqual,
            LoxTokenType::Greater => OwnedLoxTokenType::Greater,
            LoxTokenType::GreaterEqual => OwnedLoxTokenType::GreaterEqual,
            LoxTokenType::Less => OwnedLoxTokenType::Less,
            LoxTokenType::LessEqual => OwnedLoxTokenType::LessEqual,
            LoxTokenType::Identifier => OwnedLoxTokenType::Identifier,
            LoxTokenType::String(s) => OwnedLoxTokenType::String(s.to_string()),
            LoxTokenType::Number(n) => OwnedLoxTokenType::Number(*n),
            LoxTokenType::And => OwnedLoxTokenType::And,
            LoxTokenType::Class => OwnedLoxTokenType::Class,
            LoxTokenType::Else => OwnedLoxTokenType::Else,
            LoxTokenType::False => OwnedLoxTokenType::False,
            LoxTokenType::Fun => OwnedLoxTokenType::Fun,
            LoxTokenType::For => OwnedLoxTokenType::For,
            LoxTokenType::If => OwnedLoxTokenType::If,
            LoxTokenType::Nil => OwnedLoxTokenType::Nil,
            LoxTokenType::Or => OwnedLoxTokenType::Or,
            LoxTokenType::Print => OwnedLoxTokenType::Print,
            LoxTokenType::Return => OwnedLoxTokenType::Return,
            LoxTokenType::Super => OwnedLoxTokenType::Super,
            LoxTokenType::This => OwnedLoxTokenType::This,
            LoxTokenType::True => OwnedLoxTokenType::True,
            LoxTokenType::Var => OwnedLoxTokenType::Var,
            LoxTokenType::While => OwnedLoxTokenType::While,
            LoxTokenType::EOF => OwnedLoxTokenType::EOF,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OwnedLoxToken {
    pub token_type: OwnedLoxTokenType,
    pub lexeme: String,
    pub line: usize,
}

impl<'a> From<&LoxToken<'a>> for OwnedLoxToken {
    fn from(t: &LoxToken<'a>) -> Self {
        OwnedLoxToken {
            token_type: (&t.token_type).into(),
            lexeme: t.lexeme.into(),
            line: t.line,
        }
    }
}
