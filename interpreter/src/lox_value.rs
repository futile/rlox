use lexer::{owned_token::OwnedLoxToken, LoxToken, LoxTokenType};

#[derive(Debug)]
pub enum LoxValue {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

#[derive(Debug, thiserror::Error)]
#[error("Token cannot be converted to a value: {token:?}")]
pub struct TokenToValueConversionError {
    pub token: OwnedLoxToken,
}

impl<'a> TryFrom<&LoxToken<'a>> for LoxValue {
    type Error = TokenToValueConversionError;

    fn try_from(token: &LoxToken<'a>) -> Result<Self, Self::Error> {
        match token.token_type {
            LoxTokenType::Nil => Ok(LoxValue::Nil),
            LoxTokenType::True => Ok(LoxValue::Bool(true)),
            LoxTokenType::False => Ok(LoxValue::Bool(false)),
            LoxTokenType::Number(n) => Ok(LoxValue::Number(*n)),
            LoxTokenType::String(s) => Ok(LoxValue::String(s.to_string())),
            _ => Err(TokenToValueConversionError {
                token: token.into(),
            }),
        }
    }
}
