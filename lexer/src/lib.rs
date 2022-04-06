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

        while let Some(token) = self.lex_single_token(&mut remaining_input)? {
            self.tokens.push(token);
        }

        assert!(
            remaining_input.is_empty(),
            "lexing didn't consume all input"
        );

        self.tokens
            .push(LoxToken::new(LoxTokenType::EOF, "", self.current_line));

        Ok(self.tokens)
    }

    fn lex_single_token(
        &mut self,
        input: &mut &'a str,
    ) -> Result<Option<LoxToken<'a>>, LexerError> {
        let full_input: &'a str = &**input;

        let build_token = |remaining_input: &str, token_type: LoxTokenType| -> LoxToken<'a> {
            LoxToken::new(
                token_type,
                &full_input[..full_input.len() - remaining_input.len()],
                self.current_line,
            )
        };

        let c = match take_first_char(input) {
            None => return Ok(None),
            Some(c) => c,
        };

        let new_token = match c {
            '(' => build_token(input, LoxTokenType::LeftParen),
            ')' => build_token(input, LoxTokenType::RightParen),
            '{' => build_token(input, LoxTokenType::LeftBrace),
            '}' => build_token(input, LoxTokenType::RightBrace),
            ',' => build_token(input, LoxTokenType::Comma),
            '.' => build_token(input, LoxTokenType::Dot),
            '-' => build_token(input, LoxTokenType::Minus),
            '+' => build_token(input, LoxTokenType::Plus),
            ';' => build_token(input, LoxTokenType::Semicolon),
            '*' => build_token(input, LoxTokenType::Star),
            '!' => {
                let token_type = if take_first_char_if_eq(input, '=') {
                    LoxTokenType::BangEqual
                } else {
                    LoxTokenType::Bang
                };
                build_token(input, token_type)
            }
            '=' => {
                let token_type = if take_first_char_if_eq(input, '=') {
                    LoxTokenType::EqualEqual
                } else {
                    LoxTokenType::Equal
                };
                build_token(input, token_type)
            }
            '<' => {
                let token_type = if take_first_char_if_eq(input, '=') {
                    LoxTokenType::LessEqual
                } else {
                    LoxTokenType::Less
                };
                build_token(input, token_type)
            }
            '>' => {
                let token_type = if take_first_char_if_eq(input, '=') {
                    LoxTokenType::GreaterEqual
                } else {
                    LoxTokenType::Greater
                };
                build_token(input, token_type)
            }
            '/' => {
                todo!()
            }
            _ => panic!("unexpected character: {c:?}"),
        };

        Ok(Some(new_token))
    }
}

/// Extract the first char from `input` and advance the slice.
fn take_first_char(input: &'_ mut &'_ str) -> Option<char> {
    take_first_char_if(input, |_| true)
}

/// Consume the first char from `input` and advance the slice if `cond(f)` is
/// true.
fn take_first_char_if(input: &'_ mut &'_ str, cond: impl FnOnce(&char) -> bool) -> Option<char> {
    let mut chars = input.chars();
    match chars.by_ref().peekable().next_if(cond) {
        res @ Some(_) => {
            *input = chars.as_str();
            res
        }
        None => None,
    }
}

/// Consume the first char from `input` and advance the slice if it matches `c`
fn take_first_char_if_eq(input: &'_ mut &'_ str, c: char) -> bool {
    take_first_char_if(input, |&v| c == v).is_some()
}

#[cfg(test)]
mod tests {
    use crate::{take_first_char, LoxLexer, LoxToken, LoxTokenType};

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

    #[test]
    fn lex_left_paren() {
        assert_eq!(
            &LoxLexer::new("(").lex_into_tokens().unwrap(),
            &[
                LoxToken {
                    token_type: LoxTokenType::LeftParen,
                    lexeme: "(",
                    line: 1
                },
                LoxToken {
                    token_type: LoxTokenType::EOF,
                    lexeme: "",
                    line: 1
                }
            ]
        );
    }

    #[test]
    fn lex_left_right_paren() {
        assert_eq!(
            &LoxLexer::new("()").lex_into_tokens().unwrap(),
            &[
                LoxToken {
                    token_type: LoxTokenType::LeftParen,
                    lexeme: "(",
                    line: 1
                },
                LoxToken {
                    token_type: LoxTokenType::RightParen,
                    lexeme: ")",
                    line: 1
                },
                LoxToken {
                    token_type: LoxTokenType::EOF,
                    lexeme: "",
                    line: 1
                }
            ]
        );
    }

    #[test]
    fn lex_single_chars() {
        assert_eq!(
            LoxLexer::new("(){},.-+;*=!<>")
                .lex_into_tokens()
                .unwrap()
                .len(),
            15
        );
    }

    #[test]
    fn lex_double_chars() {
        assert_eq!(
            LoxLexer::new("!===<=>=").lex_into_tokens().unwrap().len(),
            5
        );
    }

    #[test]
    fn take_first_char_works() {
        let s = "abc";
        let mut rem = s;

        assert_eq!(take_first_char(&mut rem), Some('a'));
        assert_eq!(take_first_char(&mut rem), Some('b'));
        assert_eq!(take_first_char(&mut rem), Some('c'));
    }
}
