use thiserror::Error;

#[derive(Debug, PartialEq, Eq)]
pub enum LoxTokenType {
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
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    NIL,
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

#[derive(Debug, PartialEq, Eq)]
pub struct LoxToken<'a> {
    pub token_type: LoxTokenType,
    pub lexeme: &'a str,
    pub line: usize,
}

impl LoxToken<'_> {
    pub fn new(token_type: LoxTokenType, lexeme: &str, line: usize) -> LoxToken {
        LoxToken {
            token_type,
            lexeme,
            line,
        }
    }
}

#[derive(Error, Debug)]
pub enum LexerError {}

#[derive(Debug, Default)]
pub struct LoxLexer<'a> {
    input: &'a str,
    current_line: usize,
    tokens: Vec<LoxToken<'a>>,
}

impl<'a> LoxLexer<'a> {
    pub fn new(input: &str) -> LoxLexer {
        LoxLexer {
            input,
            current_line: 1,
            tokens: Vec::new(),
        }
    }

    pub fn lex_into_tokens(mut self) -> Result<Vec<LoxToken<'a>>, LexerError> {
        let mut remaining_input = self.input;

        while !remaining_input.is_empty() {
            self.tokens
                .push(Self::lex_single_token(&mut remaining_input)?);
        }

        self.tokens
            .push(LoxToken::new(LoxTokenType::EOF, "", self.current_line));

        Ok(self.tokens)
    }

    fn lex_single_token(_input_cursor: &mut &'a str) -> Result<LoxToken<'a>, LexerError> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{LoxLexer, LoxToken, LoxTokenType};

    #[test]
    fn lex_empty_string() {
        assert_eq!(
            &LoxLexer::new("").lex_into_tokens().unwrap(),
            &[LoxToken {
                token_type: LoxTokenType::EOF,
                lexeme: "",
                line: 1
            }]
        );
    }
}
