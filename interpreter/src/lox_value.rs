use lexer::{owned_token::OwnedLoxToken, LoxToken, LoxTokenType};

#[derive(Debug, Clone)]
pub enum LoxValue {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

#[derive(Debug, thiserror::Error)]
#[error("Could not negate value because its not a number: {value:?}")]
pub struct NumberNegationError {
    value: LoxValue,
}

#[derive(Debug, thiserror::Error)]
#[error("Could not invert value because its not a bool: {value:?}")]
pub struct BoolInversionError {
    value: LoxValue,
}

impl LoxValue {
    pub fn number_negated(&self) -> Result<LoxValue, NumberNegationError> {
        if let LoxValue::Number(ref n) = self {
            Ok(LoxValue::Number(-n))
        } else {
            Err(NumberNegationError {
                value: self.clone(),
            })
        }
    }

    pub fn truthy(&self) -> LoxValue {
        match self {
            LoxValue::Bool(..) => self.clone(),
            LoxValue::Nil => LoxValue::Bool(false),
            _ => LoxValue::Bool(true),
        }
    }

    pub fn bool_inverted(&self) -> Result<LoxValue, BoolInversionError> {
        if let LoxValue::Bool(b) = self {
            Ok(LoxValue::Bool(!b))
        } else {
            Err(BoolInversionError {
                value: self.clone(),
            })
        }
    }
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
