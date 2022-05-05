use lexer::{owned_token::OwnedLoxToken, LoxToken, LoxTokenType};

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

#[derive(Debug, thiserror::Error)]
#[error("Could not negate value because it's not a number: {value:?}")]
pub struct NumberNegationError {
    value: LoxValue,
}

#[derive(Debug, thiserror::Error)]
#[error("Could not invert value because it's not a bool: {value:?}")]
pub struct BoolInversionError {
    value: LoxValue,
}

#[derive(Debug, thiserror::Error)]
#[error("Operation \"{op}\" failed, lhs: {lhs:?}, rhs: {rhs:?}")]
pub struct BinaryOpError {
    op: &'static str,
    lhs: LoxValue,
    rhs: LoxValue,
}

impl LoxValue {
    pub fn number(&self) -> Option<f64> {
        if let LoxValue::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    pub fn number_negated(&self) -> Result<LoxValue, NumberNegationError> {
        self.number().map_or_else(
            || {
                Err(NumberNegationError {
                    value: self.clone(),
                })
            },
            |n| Ok(LoxValue::Number(-n)),
        )
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

    fn try_binary_op<T>(
        &self,
        rhs: &LoxValue,
        op: impl for<'a, 'b> FnOnce(&'a f64, &'b f64) -> T,
        op_str: &'static str,
    ) -> Result<T, BinaryOpError> {
        match (self, rhs) {
            (LoxValue::Number(ln), LoxValue::Number(rn)) => Ok(op(ln, rn)),
            _ => Err(BinaryOpError {
                op: op_str,
                lhs: self.clone(),
                rhs: rhs.clone(),
            }),
        }
    }

    pub fn try_add(&self, rhs: &LoxValue) -> Result<LoxValue, BinaryOpError> {
        if let Ok(v) = self.try_binary_op(rhs, |l, r| l + r, "+") {
            return Ok(LoxValue::Number(v));
        }

        match (self, rhs) {
            (LoxValue::String(lhs), LoxValue::String(rhs)) => {
                return Ok(LoxValue::String(format!("{lhs}{rhs}")))
            }
            _ => Err(BinaryOpError {
                op: "+",
                lhs: self.clone(),
                rhs: rhs.clone(),
            }),
        }
    }

    pub fn try_sub(&self, rhs: &LoxValue) -> Result<LoxValue, BinaryOpError> {
        self.try_binary_op(rhs, |l, r| l - r, "-")
            .map(LoxValue::Number)
    }

    pub fn try_mul(&self, rhs: &LoxValue) -> Result<LoxValue, BinaryOpError> {
        self.try_binary_op(rhs, |l, r| l * r, "*")
            .map(LoxValue::Number)
    }

    pub fn try_div(&self, rhs: &LoxValue) -> Result<LoxValue, BinaryOpError> {
        self.try_binary_op(rhs, |l, r| l / r, "/")
            .map(LoxValue::Number)
    }

    pub fn try_greater(&self, rhs: &LoxValue) -> Result<LoxValue, BinaryOpError> {
        self.try_binary_op(rhs, PartialOrd::gt, ">")
            .map(LoxValue::Bool)
    }

    pub fn try_greater_equal(&self, rhs: &LoxValue) -> Result<LoxValue, BinaryOpError> {
        self.try_binary_op(rhs, PartialOrd::ge, ">=")
            .map(LoxValue::Bool)
    }

    pub fn try_less(&self, rhs: &LoxValue) -> Result<LoxValue, BinaryOpError> {
        self.try_binary_op(rhs, PartialOrd::lt, "<")
            .map(LoxValue::Bool)
    }

    pub fn try_less_equal(&self, rhs: &LoxValue) -> Result<LoxValue, BinaryOpError> {
        self.try_binary_op(rhs, PartialOrd::le, "<=")
            .map(LoxValue::Bool)
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
