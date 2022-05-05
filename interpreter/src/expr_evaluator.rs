use lexer::LoxTokenType;
use parser::expr::ExprVisitor;

use crate::lox_value::{
    BinaryOpError, BoolInversionError, LoxValue, NumberNegationError, TokenToValueConversionError,
};

#[derive(Debug, thiserror::Error)]
pub enum ExprEvaluationError {
    #[error("Could not evaluate literal expression: {source}")]
    LiteralExprEvaluationFailed { source: TokenToValueConversionError },
    #[error(transparent)]
    NegationFailed(NumberNegationError),
    #[error(transparent)]
    InversionFailed(BoolInversionError),
    #[error(transparent)]
    BinaryOpFailed(BinaryOpError),
}

pub struct ExprEvaluator;

impl ExprVisitor for ExprEvaluator {
    type Output = Result<LoxValue, ExprEvaluationError>;

    fn visit_ternary_expr(&mut self, _expr: &parser::expr::TernaryExpr<'_>) -> Self::Output {
        todo!()
    }

    fn visit_binary_expr(&mut self, expr: &parser::expr::BinaryExpr<'_>) -> Self::Output {
        let lhs = expr.left.accept(self)?;
        let rhs = expr.right.accept(self)?;

        match expr.operator.token_type {
            LoxTokenType::Plus => lhs
                .try_add(&rhs)
                .map_err(ExprEvaluationError::BinaryOpFailed),
            LoxTokenType::Minus => lhs
                .try_sub(&rhs)
                .map_err(ExprEvaluationError::BinaryOpFailed),
            LoxTokenType::Star => lhs
                .try_mul(&rhs)
                .map_err(ExprEvaluationError::BinaryOpFailed),
            LoxTokenType::Slash => lhs
                .try_div(&rhs)
                .map_err(ExprEvaluationError::BinaryOpFailed),
            ref tt => panic!("unexpected operator {tt:?} in binary expression: {expr:?}"),
        }
    }

    fn visit_unary_expr(&mut self, expr: &parser::expr::UnaryExpr<'_>) -> Self::Output {
        let value = expr.right.accept(self)?;

        match expr.operator.token_type {
            LoxTokenType::Bang => value
                .truthy()
                .bool_inverted()
                .map_err(ExprEvaluationError::InversionFailed),
            LoxTokenType::Minus => value
                .number_negated()
                .map_err(ExprEvaluationError::NegationFailed),
            _ => panic!("unexpected operator in unary expression: {expr:?}"),
        }
    }

    fn visit_grouping_expr(&mut self, expr: &parser::expr::GroupingExpr<'_>) -> Self::Output {
        expr.inner.accept(self)
    }

    fn visit_literal_expr(&mut self, expr: &'_ parser::expr::LiteralExpr<'_>) -> Self::Output {
        LoxValue::try_from(&expr.literal)
            .map_err(|e| ExprEvaluationError::LiteralExprEvaluationFailed { source: e })
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use lexer::owned_token::{OwnedLoxToken, OwnedLoxTokenType};
    use parser::parser_from_str;

    use super::{ExprEvaluationError, ExprEvaluator};
    use crate::lox_value::{LoxValue, TokenToValueConversionError};

    fn evaluate_str(input: &str) -> Result<LoxValue, ExprEvaluationError> {
        let expr = parser_from_str(input).unwrap().parse().unwrap();
        expr.accept(&mut ExprEvaluator)
    }

    fn evaluate_expect_number(input: &str, expected_val: f64) {
        let res = evaluate_str(input).unwrap();
        let LoxValue::Number(n) = res else { panic!("expected number, but got: {res:?}") };
        assert_eq!(n, expected_val);
    }

    #[test]
    fn evaluate_literal_number() {
        evaluate_expect_number("1", 1.0);
    }

    #[test]
    fn evaluate_literal_string() {
        let res = evaluate_str("\"foobar\"").unwrap();
        let LoxValue::String(ref s) = res else { panic!("expected string, but got: {res:?}") };
        assert_eq!(s, "foobar");
    }

    #[test]
    fn evaluate_literal_bool() {
        let res = evaluate_str("true").unwrap();
        let LoxValue::Bool(b) = res else { panic!("expected bool, but got: {res:?}") };
        assert_eq!(b, true);
    }

    #[test]
    fn evaluate_literal_nil() {
        let res = evaluate_str("nil").unwrap();
        assert_matches!(res, LoxValue::Nil);
    }

    #[test]
    fn evaluate_unary_bang() {
        let res = evaluate_str("!true").unwrap();
        let LoxValue::Bool(b) = res else { panic!("expected bool, but got: {res:?}") };
        assert_eq!(b, false);

        let res = evaluate_str("!nil").unwrap();
        let LoxValue::Bool(b) = res else { panic!("expected bool, but got: {res:?}") };
        assert_eq!(b, true);
    }

    #[test]
    fn evaluate_unary_minus() {
        evaluate_expect_number("-1", -1.0);
    }

    #[test]
    fn evaluate_binary_nums() {
        evaluate_expect_number("10 + 2", 12.0);
        evaluate_expect_number("10 - 2", 8.0);
        evaluate_expect_number("10 * 2", 20.0);
        evaluate_expect_number("10 / 2", 5.0);
    }

    #[test]
    fn evaluate_binary_strings() {
        let res = evaluate_str("\"a\" + \"b\"").unwrap();
        let LoxValue::String(s) = res else { panic!("expected string, but got: {res:?}") };
        assert_eq!(s, "ab");
    }

    #[test]
    fn evaluate_grouping() {
        let res = evaluate_str("(nil)").unwrap();
        assert_matches!(res, LoxValue::Nil);
    }

    #[test]
    fn foobar() {
        let e = ExprEvaluationError::LiteralExprEvaluationFailed {
            source: TokenToValueConversionError {
                token: OwnedLoxToken {
                    token_type: OwnedLoxTokenType::Nil,
                    lexeme: "nil".to_string(),
                    line: 1,
                },
            },
        };
        eprintln!("{e}");
    }
}
