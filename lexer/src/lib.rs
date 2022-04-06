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
pub enum LexerError {}

#[derive(Debug, Default)]
pub struct LoxLexer<'a> {
    input: &'a str,
    current_line: usize,
    tokens: Vec<LoxToken<'a>>,
}

impl<'a> LoxLexer<'a> {
    pub fn new(input: &'a str) -> LoxLexer {
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

    fn lex_single_token<'i>(
        &mut self,
        input: &mut &'i str,
    ) -> Result<Option<LoxToken<'i>>, LexerError> {
        let mut full_input: &'i str = &**input;

        macro_rules! build_token {
            ($token_type:expr, line = $line:expr) => {
                LoxToken::new(
                    $token_type,
                    &full_input[..full_input.len() - input.len()],
                    $line,
                )
            };
            ($token_type:expr) => {
                build_token!($token_type, line = self.current_line)
            };
        }

        let new_token = 'token_loop: loop {
            let c = match take_first_char(input) {
                None => return Ok(None),
                Some(c) => c,
            };

            match c {
                '(' => break build_token!(LoxTokenType::LeftParen),
                ')' => break build_token!(LoxTokenType::RightParen),
                '{' => break build_token!(LoxTokenType::LeftBrace),
                '}' => break build_token!(LoxTokenType::RightBrace),
                ',' => break build_token!(LoxTokenType::Comma),
                '.' => break build_token!(LoxTokenType::Dot),
                '-' => break build_token!(LoxTokenType::Minus),
                '+' => break build_token!(LoxTokenType::Plus),
                ';' => break build_token!(LoxTokenType::Semicolon),
                '*' => break build_token!(LoxTokenType::Star),
                '!' => {
                    let token_type = if take_first_char_if_eq(input, '=') {
                        LoxTokenType::BangEqual
                    } else {
                        LoxTokenType::Bang
                    };
                    break build_token!(token_type);
                }
                '=' => {
                    let token_type = if take_first_char_if_eq(input, '=') {
                        LoxTokenType::EqualEqual
                    } else {
                        LoxTokenType::Equal
                    };
                    break build_token!(token_type);
                }
                '<' => {
                    let token_type = if take_first_char_if_eq(input, '=') {
                        LoxTokenType::LessEqual
                    } else {
                        LoxTokenType::Less
                    };
                    break build_token!(token_type);
                }
                '>' => {
                    let token_type = if take_first_char_if_eq(input, '=') {
                        LoxTokenType::GreaterEqual
                    } else {
                        LoxTokenType::Greater
                    };
                    break build_token!(token_type);
                }
                '/' => {
                    if take_first_char_if_eq(input, '/') {
                        // skip rest of comment
                        while take_first_char_if(input, |&c| c != '\n').is_some() {}
                    } else {
                        break build_token!(LoxTokenType::Slash);
                    }
                }
                '"' => {
                    let starting_line = self.current_line;
                    let mut token = loop {
                        match take_first_char(input) {
                            Some('"') => {
                                break build_token!(LoxTokenType::String(""), line = starting_line)
                            }
                            Some('\n') => self.current_line += 1,
                            Some(_) => (), // consume
                            None => panic!("unterminated string (line {})", self.current_line),
                        };
                    };

                    // strip surrounding ""
                    token.token_type =
                        LoxTokenType::String(&token.lexeme[1..token.lexeme.len() - 1]);

                    break token;
                }
                c if c.is_ascii_digit() => {
                    let mut dot_seen = false;
                    loop {
                        match take_first_char_if_lookahead_2(input, |&c1, &c2| {
                            matches!((c1, c2), ('0'..='9', _) | ('.', Some('0'..='9')))
                        }) {
                            Some('.') => {
                                if dot_seen {
                                    panic!(
                                        "dot appears twice in float literal (line {})",
                                        self.current_line
                                    );
                                } else {
                                    dot_seen = true;
                                }
                            }
                            Some(_) => (), // consume
                            None => {
                                let mut token =
                                    build_token!(LoxTokenType::Number(NotNan::default()));

                                let val = token.lexeme.parse::<f64>().unwrap_or_else(|e| {
                                    panic!(
                                        "could not parse number (line {}): {e:?}",
                                        self.current_line
                                    )
                                });

                                token.token_type =
                                    LoxTokenType::Number(NotNan::new(val).unwrap_or_else(|e| {
                                        panic!(
                                            "parsed number was somehow NaN (line {}): {e:?}",
                                            self.current_line
                                        )
                                    }));

                                break 'token_loop token;
                            }
                        };
                    }
                }
                ' ' | '\r' | '\t' => full_input = input, // ignore whitespace
                '\n' => {
                    self.current_line += 1;
                    full_input = input;
                }
                _ => panic!("unexpected character: {c:?}"),
            };
        };

        Ok(Some(new_token))
    }
}

/// Extract the first char from `input` and advance the slice.
fn take_first_char(input: &'_ mut &'_ str) -> Option<char> {
    take_first_char_if(input, |_| true)
}

/// Consume the first char from `input` and advance the slice if
/// `cond(first_char)` is true.
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

/// Consume the first char from `input` and advance the slice if
/// `cond(first_char, second_char)` is true.
fn take_first_char_if_lookahead_2(
    input: &'_ mut &'_ str,
    cond: impl FnOnce(&char, &Option<char>) -> bool,
) -> Option<char> {
    let mut chars = input.chars();

    let c1 = match chars.next() {
        Some(c) => c,
        None => return None,
    };

    let c2 = chars.next();

    let mut chars = input.chars();

    if cond(&c1, &c2) {
        chars.next();
        *input = chars.as_str();

        Some(c1)
    } else {
        None
    }
}

/// Consume the first char from `input` and advance the slice if it matches `c`
fn take_first_char_if_eq(input: &'_ mut &'_ str, c: char) -> bool {
    take_first_char_if(input, |&v| c == v).is_some()
}

#[cfg(test)]
mod tests {
    use ordered_float::NotNan;

    use crate::{take_first_char, LoxLexer, LoxToken, LoxTokenType};

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
    #[should_panic]
    fn lex_unterminated_string() {
        LoxLexer::new("\"").lex_into_tokens().unwrap();
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
    fn take_first_char_works() {
        let s = "abc";
        let mut rem = s;

        assert_eq!(take_first_char(&mut rem), Some('a'));
        assert_eq!(take_first_char(&mut rem), Some('b'));
        assert_eq!(take_first_char(&mut rem), Some('c'));
    }
}
