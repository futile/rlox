use thiserror::Error;

#[derive(Debug, PartialEq, Eq)]
pub enum LoxToken {}

#[derive(Error, Debug)]
pub enum LexerError {}

#[derive(Debug, Default)]
pub struct LoxLexer {}

impl LoxLexer {
    pub fn new() -> LoxLexer {
        LoxLexer {}
    }

    pub fn lex_into_tokens(self, _input: &str) -> Result<Vec<LoxToken>, LexerError> {
        Ok(Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use crate::LoxLexer;

    #[test]
    fn lex_empty_string() {
        assert_eq!(LoxLexer::new().lex_into_tokens("").unwrap(), vec![]);
    }
}
