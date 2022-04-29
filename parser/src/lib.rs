#![feature(assert_matches)]

use std::iter::{Fuse, Peekable};

use expr::{BinaryExpr, GroupingExpr, LiteralExpr, LoxExpr, TernaryExpr, UnaryExpr};
use lexer::{LexerError, LoxLexer, LoxToken, LoxTokenType};
use thiserror::Error;

pub mod ast_printer;
pub mod expr;

#[derive(Debug)]
pub struct LoxParser<'a, I>
where
    I: Iterator<Item = LoxToken<'a>>,
{
    tokens: Peekable<Fuse<I>>,
}

impl<'a, I> LoxParser<'a, I>
where
    I: Iterator<Item = LoxToken<'a>>,
{
    pub fn new(tokens: I) -> LoxParser<'a, I> {
        LoxParser {
            tokens: tokens.fuse().peekable(),
        }
    }

    pub fn parse(mut self) -> Result<LoxExpr<'a>, ParserError> {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> Result<LoxExpr<'a>, ParserError> {
        self.parse_comma()
    }

    fn parse_comma(&mut self) -> Result<LoxExpr<'a>, ParserError> {
        let mut expr = self.parse_ternary()?;

        while let Some(op) = self.tokens.next_if(|t| t.token_type == LoxTokenType::Comma) {
            let right = self.parse_ternary()?;
            expr = BinaryExpr::new(expr, op, right).into_expr();
        }

        Ok(expr)
    }

    fn parse_ternary(&mut self) -> Result<LoxExpr<'a>, ParserError> {
        let mut expr = self.parse_equality()?;

        while let Some(question_op) = self
            .tokens
            .next_if(|t| t.token_type == LoxTokenType::Questionmark)
        {
            // inner expr (between '?' and ':' is parsed as if grouped in parentheses, like
            // in C)
            let inner_expr = self.parse_expression()?;

            let colon_op = self
                .tokens
                .next_if(|t| t.token_type == LoxTokenType::Colon)
                .ok_or(ParserError::MissingTernaryColon {
                    starting_line: question_op.line,
                })?;

            // right-associativity, like in C
            let right = self.parse_ternary()?;
            expr = TernaryExpr::new(expr, question_op, inner_expr, colon_op, right).into_expr();
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<LoxExpr<'a>, ParserError> {
        let mut expr = self.parse_comparison()?;

        while let Some(op) = self.tokens.next_if(|t| {
            matches!(
                t.token_type,
                LoxTokenType::BangEqual | LoxTokenType::EqualEqual
            )
        }) {
            let right = self.parse_comparison()?;
            expr = BinaryExpr::new(expr, op, right).into_expr();
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<LoxExpr<'a>, ParserError> {
        let mut expr = self.parse_term()?;

        while let Some(op) = self.tokens.next_if(|t| {
            matches!(
                t.token_type,
                LoxTokenType::Greater
                    | LoxTokenType::GreaterEqual
                    | LoxTokenType::Less
                    | LoxTokenType::LessEqual
            )
        }) {
            let right = self.parse_term()?;
            expr = BinaryExpr::new(expr, op, right).into_expr();
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<LoxExpr<'a>, ParserError> {
        let mut expr = self.parse_factor()?;

        while let Some(op) = self
            .tokens
            .next_if(|t| matches!(t.token_type, LoxTokenType::Minus | LoxTokenType::Plus))
        {
            let right = self.parse_factor()?;
            expr = BinaryExpr::new(expr, op, right).into_expr();
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<LoxExpr<'a>, ParserError> {
        let mut expr = self.parse_unary()?;

        while let Some(op) = self
            .tokens
            .next_if(|t| matches!(t.token_type, LoxTokenType::Slash | LoxTokenType::Star))
        {
            let right = self.parse_unary()?;
            expr = BinaryExpr::new(expr, op, right).into_expr();
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<LoxExpr<'a>, ParserError> {
        if let Some(op) = self
            .tokens
            .next_if(|t| matches!(t.token_type, LoxTokenType::Bang | LoxTokenType::Minus))
        {
            let right = self.parse_unary()?;
            return Ok(UnaryExpr::new(op, right).into_expr());
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<LoxExpr<'a>, ParserError> {
        if let Some(t) = self.tokens.next_if(|t| {
            matches!(
                t.token_type,
                LoxTokenType::False
                    | LoxTokenType::True
                    | LoxTokenType::Nil
                    | LoxTokenType::Number(..)
                    | LoxTokenType::String(..)
            )
        }) {
            return Ok(LiteralExpr::new(t).into_expr());
        }

        if let Some(lparen) = self
            .tokens
            .next_if(|t| t.token_type == LoxTokenType::LeftParen)
        {
            let inner = self.parse_expression()?;
            self.tokens
                .next_if(|t| t.token_type == LoxTokenType::RightParen)
                .ok_or(ParserError::MissingRightParen {
                    starting_line: lparen.line,
                })?;
            return Ok(GroupingExpr::new(inner).into_expr());
        }

        let next = self
            .tokens
            .peek()
            .expect("somehow moved past end of tokens");

        Err(ParserError::MissingExpression {
            line: next.line,
            lexeme: next.lexeme.to_string(),
        })
    }

    // unused for now in the book
    #[allow(unused)]
    fn synchronize(&mut self) {
        while let Some(t) = self.tokens.next() {
            if t.token_type == LoxTokenType::Semicolon {
                return;
            }

            let peek_type = match self.tokens.peek() {
                Some(t) => &t.token_type,
                None => return,
            };

            match peek_type {
                LoxTokenType::Class
                | LoxTokenType::Fun
                | LoxTokenType::Var
                | LoxTokenType::For
                | LoxTokenType::If
                | LoxTokenType::While
                | LoxTokenType::Print
                | LoxTokenType::Return => return,
                _ => (),
            }
        }
    }
}

pub fn parser_from_str(
    input: &str,
) -> Result<LoxParser<'_, impl Iterator<Item = LoxToken<'_>>>, LexerError> {
    Ok(LoxParser::new(
        LoxLexer::new(input).lex_into_tokens()?.into_iter(),
    ))
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Expected ')' after expression, opening '(' in line {starting_line}")]
    MissingRightParen { starting_line: usize },
    #[error("Expected expression (line {line}, lexeme {lexeme:?})")]
    MissingExpression { line: usize, lexeme: String },
    #[error("Expected ':' after expression, opening '?' in line {starting_line}")]
    MissingTernaryColon { starting_line: usize },
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use lexer::{LoxToken, LoxTokenType, NotNan};

    use crate::{
        expr::{BinaryExpr, GroupingExpr, LiteralExpr, LoxExpr, TernaryExpr, UnaryExpr},
        parser_from_str, LoxParser, ParserError,
    };

    #[test]
    fn it_works() {
        let t: [LoxToken<'static>; 0] = [];
        let _ = LoxParser::new(t.into_iter());
        let _ = parser_from_str("").unwrap();
    }

    #[test]
    fn parse_simple_expr() {
        let res = parser_from_str("-123 * (45.67)")
            .unwrap()
            .parse_expression()
            .unwrap();

        let expected_expr = LoxExpr::Binary(BinaryExpr::new(
            LoxExpr::Unary(UnaryExpr::new(
                LoxToken::new(LoxTokenType::Minus, "-", 1),
                LoxExpr::Literal(LiteralExpr::new(LoxToken::new(
                    LoxTokenType::Number(NotNan::new(123.0).unwrap()),
                    "123",
                    1,
                ))),
            )),
            LoxToken::new(LoxTokenType::Star, "*", 1),
            LoxExpr::Grouping(GroupingExpr::new(LoxExpr::Literal(LiteralExpr::new(
                LoxToken::new(
                    LoxTokenType::Number(NotNan::new(45.67).unwrap()),
                    "45.67",
                    1,
                ),
            )))),
        ));

        assert_eq!(res, expected_expr);
    }

    #[test]
    fn parse_comma_expr() {
        let res = parser_from_str("1,2,3")
            .unwrap()
            .parse_expression()
            .unwrap();

        let expected_expr = BinaryExpr::new(
            BinaryExpr::new(
                LiteralExpr::new(LoxToken::new(
                    LoxTokenType::Number(NotNan::new(1.0).unwrap()),
                    "1",
                    1,
                ))
                .into_expr(),
                LoxToken::new(LoxTokenType::Comma, ",", 1),
                LiteralExpr::new(LoxToken::new(
                    LoxTokenType::Number(NotNan::new(2.0).unwrap()),
                    "2",
                    1,
                ))
                .into_expr(),
            )
            .into_expr(),
            LoxToken::new(LoxTokenType::Comma, ",", 1),
            LiteralExpr::new(LoxToken::new(
                LoxTokenType::Number(NotNan::new(3.0).unwrap()),
                "3",
                1,
            ))
            .into_expr(),
        )
        .into_expr();

        assert_eq!(res, expected_expr);
    }

    #[test]
    fn parse_ternary_expr() {
        let res = parser_from_str("1 ? 2, 2.5 : 3 ? 4 : 5")
            .unwrap()
            .parse_expression()
            .unwrap();

        let expected_expr = TernaryExpr::new(
            LiteralExpr::new(LoxToken::new(
                LoxTokenType::Number(NotNan::new(1.0).unwrap()),
                "1",
                1,
            ))
            .into_expr(),
            LoxToken::new(LoxTokenType::Questionmark, "?", 1),
            BinaryExpr::new(
                LiteralExpr::new(LoxToken::new(
                    LoxTokenType::Number(NotNan::new(2.0).unwrap()),
                    "2",
                    1,
                ))
                .into_expr(),
                LoxToken::new(LoxTokenType::Comma, ",", 1),
                LiteralExpr::new(LoxToken::new(
                    LoxTokenType::Number(NotNan::new(2.5).unwrap()),
                    "2.5",
                    1,
                ))
                .into_expr(),
            )
            .into_expr(),
            LoxToken::new(LoxTokenType::Colon, ":", 1),
            TernaryExpr::new(
                LiteralExpr::new(LoxToken::new(
                    LoxTokenType::Number(NotNan::new(3.0).unwrap()),
                    "3",
                    1,
                ))
                .into_expr(),
                LoxToken::new(LoxTokenType::Questionmark, "?", 1),
                LiteralExpr::new(LoxToken::new(
                    LoxTokenType::Number(NotNan::new(4.0).unwrap()),
                    "4",
                    1,
                ))
                .into_expr(),
                LoxToken::new(LoxTokenType::Colon, ":", 1),
                LiteralExpr::new(LoxToken::new(
                    LoxTokenType::Number(NotNan::new(5.0).unwrap()),
                    "5",
                    1,
                ))
                .into_expr(),
            )
            .into_expr(),
        )
        .into_expr();

        assert_eq!(res, expected_expr);
    }

    #[test]
    fn missing_rparen() {
        let res = parser_from_str("(1").unwrap().parse_expression();
        assert_matches!(res, Err(ParserError::MissingRightParen { .. }));
    }

    #[test]
    fn missing_expression() {
        let res = parser_from_str("").unwrap().parse_expression();
        assert_matches!(res, Err(ParserError::MissingExpression { .. }));
    }
}
