use std::str::Chars;

use ordered_float::NotNan;
use thiserror::Error;

#[derive(Debug, PartialEq, Eq)]
pub enum LoxTokenType<'a> {
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
    String(&'a str),
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

/// Try to parse a Lox keyword from a string, returns `None` if `input` is not a
/// keyword
fn try_str_to_keyword(input: &str) -> Option<LoxTokenType> {
    match input {
        "and" => Some(LoxTokenType::And),
        "class" => Some(LoxTokenType::Class),
        "else" => Some(LoxTokenType::Else),
        "false" => Some(LoxTokenType::False),
        "for" => Some(LoxTokenType::For),
        "fun" => Some(LoxTokenType::Fun),
        "if" => Some(LoxTokenType::If),
        "nil" => Some(LoxTokenType::Nil),
        "or" => Some(LoxTokenType::Or),
        "print" => Some(LoxTokenType::Print),
        "return" => Some(LoxTokenType::Return),
        "super" => Some(LoxTokenType::Super),
        "this" => Some(LoxTokenType::This),
        "true" => Some(LoxTokenType::True),
        "var" => Some(LoxTokenType::Var),
        "while" => Some(LoxTokenType::While),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LoxToken<'a> {
    pub token_type: LoxTokenType<'a>,
    pub lexeme: &'a str,
    pub line: usize,
}

impl<'a> LoxToken<'a> {
    pub fn new(token_type: LoxTokenType<'a>, lexeme: &'a str, line: usize) -> LoxToken<'a> {
        LoxToken {
            token_type,
            lexeme,
            line,
        }
    }
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("unterminated string (starting in line {starting_line})")]
    UnterminatedString { starting_line: usize },
    #[error("unparseable number (line {line}): {source}")]
    InvalidNumber {
        line: usize,
        source: std::num::ParseFloatError,
    },
    #[error("unexpected character (line {line}): {character:?}")]
    UnexpectedCharacter { line: usize, character: char },
}

#[derive(Debug, Default)]
pub struct LoxLexer<'a> {
    remaining_input: &'a str,
    current_advance: &'a str,
    current_line: usize,
    tokens: Vec<LoxToken<'a>>,
}

impl<'a> LoxLexer<'a> {
    pub fn new(input: &'a str) -> LoxLexer {
        LoxLexer {
            remaining_input: input,
            current_advance: input,
            current_line: 1,
            tokens: Vec::new(),
        }
    }

    pub fn lex_into_tokens(mut self) -> Result<Vec<LoxToken<'a>>, LexerError> {
        while let Some(token) = self.lex_single_token()? {
            self.tokens.push(token);
        }

        assert!(
            self.remaining_input.is_empty(),
            "lexing didn't consume all input; remaining: {:?}",
            self.remaining_input
        );

        self.tokens
            .push(LoxToken::new(LoxTokenType::EOF, "", self.current_line));

        Ok(self.tokens)
    }

    fn advance_if_n(&mut self, cond: impl FnOnce(Chars) -> bool) -> Option<char> {
        if cond(self.current_advance.chars()) {
            // extract next char
            let mut chars = self.current_advance.chars();
            let c = chars.next();

            // check if the char was a newline, and if so, handle it
            if c == Some('\n') {
                self.current_line += 1;
            }

            // and advance to after this char
            self.current_advance = chars.as_str();

            c
        } else {
            None
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.advance_if_n(|_| true)
    }

    fn advance_if(&mut self, cond: impl FnOnce(char) -> bool) -> Option<char> {
        self.advance_if_n(|mut chars| chars.next().map(cond) == Some(true))
    }

    fn advance_if_eq(&mut self, c: char) -> bool {
        self.advance_if(|c2| c == c2).is_some()
    }

    fn current_lexeme(&self) -> &'a str {
        &self.remaining_input[..self.remaining_input.len() - self.current_advance.len()]
    }

    fn build_token_with_line(&mut self, token_type: LoxTokenType<'a>, line: usize) -> LoxToken<'a> {
        let lexeme = self.current_lexeme();
        self.remaining_input = self.current_advance;
        LoxToken::new(token_type, lexeme, line)
    }

    fn build_token(&mut self, token_type: LoxTokenType<'a>) -> LoxToken<'a> {
        self.build_token_with_line(token_type, self.current_line)
    }

    fn skip_current_lexeme(&mut self) {
        self.remaining_input = self.current_advance;
    }

    fn lex_number(&mut self) -> Result<LoxToken<'a>, LexerError> {
        let mut dot_seen = false;
        loop {
            match self.advance_if_n(|mut chars| {
                matches!(
                    (chars.next(), chars.next()),
                    (Some('0'..='9'), _) | (Some('.'), Some('0'..='9'))
                )
            }) {
                c @ (None | Some('.')) => {
                    if matches!(c, Some('.')) && !dot_seen {
                        dot_seen = true;
                        continue;
                    }

                    let number = self.current_lexeme().parse::<f64>().map_err(|e| {
                        LexerError::InvalidNumber {
                            line: self.current_line,
                            source: e,
                        }
                    })?;

                    break Ok(self.build_token(LoxTokenType::Number(
                        NotNan::new(number).unwrap_or_else(|e| {
                            panic!(
                                "parsed number was somehow NaN (line {}): {e:?}",
                                self.current_line
                            )
                        }),
                    )));
                }
                Some(_) => (), // consume
            };
        }
    }

    fn lex_string(&mut self) -> Result<LoxToken<'a>, LexerError> {
        assert_eq!(self.current_lexeme(), "\"");

        let starting_line = self.current_line;
        loop {
            match self.advance() {
                Some('"') => {
                    let lexeme = self.current_lexeme();
                    // strip leading and trailing ""
                    let stripped_lexeme = &lexeme[1..lexeme.len() - 1];
                    break Ok(self.build_token_with_line(
                        LoxTokenType::String(stripped_lexeme),
                        starting_line,
                    ));
                }
                Some(_) => (), // consume
                None => return Err(LexerError::UnterminatedString { starting_line }),
            }
        }
    }

    fn lex_op_with_maybe_equals(
        &mut self,
        without_equals: LoxTokenType<'a>,
        with_equals: LoxTokenType<'a>,
    ) -> LoxToken<'a> {
        let token_type = if self.advance_if_eq('=') {
            with_equals
        } else {
            without_equals
        };
        self.build_token(token_type)
    }

    fn lex_keyword_or_identifier(&mut self) -> LoxToken<'a> {
        while self
            .advance_if(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
            .is_some()
        {}

        match try_str_to_keyword(self.current_lexeme()) {
            Some(keyword) => self.build_token(keyword),
            None => self.build_token(LoxTokenType::Identifier),
        }
    }

    fn lex_single_token(&mut self) -> Result<Option<LoxToken<'a>>, LexerError> {
        let new_token = loop {
            let c = match self.advance() {
                None => return Ok(None),
                Some(c) => c,
            };

            match c {
                '(' => break self.build_token(LoxTokenType::LeftParen),
                ')' => break self.build_token(LoxTokenType::RightParen),
                '{' => break self.build_token(LoxTokenType::LeftBrace),
                '}' => break self.build_token(LoxTokenType::RightBrace),
                ',' => break self.build_token(LoxTokenType::Comma),
                '.' => break self.build_token(LoxTokenType::Dot),
                '-' => break self.build_token(LoxTokenType::Minus),
                '+' => break self.build_token(LoxTokenType::Plus),
                ';' => break self.build_token(LoxTokenType::Semicolon),
                '*' => break self.build_token(LoxTokenType::Star),
                '!' => {
                    break self
                        .lex_op_with_maybe_equals(LoxTokenType::Bang, LoxTokenType::BangEqual)
                }
                '=' => {
                    break self
                        .lex_op_with_maybe_equals(LoxTokenType::Equal, LoxTokenType::EqualEqual)
                }
                '<' => {
                    break self
                        .lex_op_with_maybe_equals(LoxTokenType::Less, LoxTokenType::LessEqual)
                }
                '>' => {
                    break self.lex_op_with_maybe_equals(
                        LoxTokenType::Greater,
                        LoxTokenType::GreaterEqual,
                    )
                }
                '/' => {
                    if self.advance_if_eq('/') {
                        // advance through rest of comment
                        while self.advance_if(|c| c != '\n').is_some() {}

                        // and skip it
                        self.skip_current_lexeme();
                    } else {
                        break self.build_token(LoxTokenType::Slash);
                    }
                }
                '"' => break self.lex_string()?,
                c if c.is_ascii_digit() => break self.lex_number()?,
                'a'..='z' | 'A'..='Z' | '_' => break self.lex_keyword_or_identifier(),
                ' ' | '\r' | '\t' | '\n' => self.skip_current_lexeme(), // ignore whitespace
                _ => {
                    return Err(LexerError::UnexpectedCharacter {
                        line: self.current_line,
                        character: c,
                    })
                }
            };
        };

        Ok(Some(new_token))
    }
}

#[cfg(test)]
mod tests {
    use ordered_float::NotNan;

    use crate::{LoxLexer, LoxToken, LoxTokenType};

    #[test]
    fn lex_empty_input() {
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
            LoxLexer::new("(){},.-+;*=!<>/")
                .lex_into_tokens()
                .unwrap()
                .len(),
            16
        );
    }

    #[test]
    fn lex_double_chars() {
        assert_eq!(
            LoxLexer::new("!= == <= >=")
                .lex_into_tokens()
                .unwrap()
                .len(),
            5
        );
    }

    #[test]
    fn lex_comment() {
        assert_eq!(LoxLexer::new("// abc").lex_into_tokens().unwrap().len(), 1);
    }

    #[test]
    fn lex_comment_and_more() {
        assert_eq!(
            LoxLexer::new("// abc\n123")
                .lex_into_tokens()
                .unwrap()
                .len(),
            2
        );
    }

    #[test]
    fn lex_whitespace() {
        assert_eq!(
            LoxLexer::new(" \r\t  \r  \t")
                .lex_into_tokens()
                .unwrap()
                .len(),
            1
        )
    }

    #[test]
    fn lex_newline() {
        assert_eq!(
            &LoxLexer::new("\n").lex_into_tokens().unwrap(),
            &[LoxToken {
                token_type: LoxTokenType::EOF,
                lexeme: "",
                line: 2
            }]
        );
    }

    #[test]
    fn lex_string() {
        let tokens = LoxLexer::new(r#" "abc" "#).lex_into_tokens().unwrap();
        assert_eq!(
            &tokens[0],
            &LoxToken {
                token_type: LoxTokenType::String("abc"),
                lexeme: r#""abc""#,
                line: 1
            }
        );
        assert_eq!(tokens.len(), 2);
    }

    #[test]
    fn lex_string_with_newline() {
        assert_eq!(
            &LoxLexer::new(" \"abc\nabc\" ").lex_into_tokens().unwrap(),
            &[
                LoxToken {
                    token_type: LoxTokenType::String("abc\nabc"),
                    lexeme: "\"abc\nabc\"",
                    line: 1
                },
                LoxToken {
                    token_type: LoxTokenType::EOF,
                    lexeme: "",
                    line: 2
                }
            ]
        );
    }

    #[test]
    fn lex_unterminated_string() {
        assert!(LoxLexer::new("\"").lex_into_tokens().is_err());
    }

    #[test]
    fn lex_number() {
        assert_eq!(
            &LoxLexer::new("123 123.04").lex_into_tokens().unwrap(),
            &[
                LoxToken {
                    token_type: LoxTokenType::Number(NotNan::new(123f64).unwrap()),
                    lexeme: "123",
                    line: 1
                },
                LoxToken {
                    token_type: LoxTokenType::Number(NotNan::new(123.04f64).unwrap()),
                    lexeme: "123.04",
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
    fn lex_number_and_dots() {
        assert_eq!(
            &LoxLexer::new(".123.").lex_into_tokens().unwrap(),
            &[
                LoxToken {
                    token_type: LoxTokenType::Dot,
                    lexeme: ".",
                    line: 1
                },
                LoxToken {
                    token_type: LoxTokenType::Number(NotNan::new(123f64).unwrap()),
                    lexeme: "123",
                    line: 1
                },
                LoxToken {
                    token_type: LoxTokenType::Dot,
                    lexeme: ".",
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
    fn lex_single_identifier() {
        assert_eq!(
            &LoxLexer::new("orchid").lex_into_tokens().unwrap(),
            &[
                LoxToken {
                    token_type: LoxTokenType::Identifier,
                    lexeme: "orchid",
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
    fn lex_some_keywords() {
        assert_eq!(
            &LoxLexer::new("or and nil print return")
                .lex_into_tokens()
                .unwrap(),
            &[
                LoxToken {
                    token_type: LoxTokenType::Or,
                    lexeme: "or",
                    line: 1
                },
                LoxToken {
                    token_type: LoxTokenType::And,
                    lexeme: "and",
                    line: 1
                },
                LoxToken {
                    token_type: LoxTokenType::Nil,
                    lexeme: "nil",
                    line: 1
                },
                LoxToken {
                    token_type: LoxTokenType::Print,
                    lexeme: "print",
                    line: 1
                },
                LoxToken {
                    token_type: LoxTokenType::Return,
                    lexeme: "return",
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
}
