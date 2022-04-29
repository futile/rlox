use parser::expr::ExprVisitor;

use crate::lox_value::{LoxValue, TokenToValueConversionError};

#[derive(Debug, thiserror::Error)]
pub enum ExprEvaluationError {
    #[error("Could not evaluate literal expression: {source}")]
    LiteralExprEvaluationFailed { source: TokenToValueConversionError },
}

pub struct ExprEvaluator;

impl ExprVisitor for ExprEvaluator {
    type Output = Result<LoxValue, ExprEvaluationError>;

    fn visit_ternary_expr(&mut self, _expr: &parser::expr::TernaryExpr<'_>) -> Self::Output {
        todo!()
    }

    fn visit_binary_expr(&mut self, _expr: &parser::expr::BinaryExpr<'_>) -> Self::Output {
        todo!()
    }

    fn visit_unary_expr(&mut self, _expr: &parser::expr::UnaryExpr<'_>) -> Self::Output {
        todo!()
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

    #[test]
    fn evaluate_literal_number() {
        let res = evaluate_str("1").unwrap();
        let LoxValue::Number(num) = res else { panic!("expected number, but got: {res:?}") };
        assert_eq!(num, 1.0);
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
