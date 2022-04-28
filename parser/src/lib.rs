#![feature(assert_matches)]

use std::iter::Peekable;

use expr::{BinaryExpr, GroupingExpr, LiteralExpr, LoxExpr, UnaryExpr};
use lexer::{LexerError, LoxLexer, LoxToken, LoxTokenType};
use thiserror::Error;

pub mod ast_printer;
pub mod expr;

#[derive(Debug)]
pub struct LoxParser<'a, I>
where
    I: Iterator<Item = LoxToken<'a>>,
{
    tokens: Peekable<I>,
}

impl<'a, I> LoxParser<'a, I>
where
    I: Iterator<Item = LoxToken<'a>>,
{
    pub fn new(tokens: I) -> LoxParser<'a, I> {
        LoxParser {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(mut self) -> Result<LoxExpr<'a>, ParserError> {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> Result<LoxExpr<'a>, ParserError> {
        self.parse_equality()
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

        if self
            .tokens
            .next_if(|t| t.token_type == LoxTokenType::LeftParen)
            .is_some()
        {
            let inner = self.parse_expression()?;
            self.tokens
                .next_if(|t| t.token_type == LoxTokenType::RightParen)
                .expect("Expected ')' after expression");
            return Ok(GroupingExpr::new(inner).into_expr());
        }

        todo!()
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
pub enum ParserError {}

#[cfg(test)]
mod tests {
    use lexer::{LoxToken, LoxTokenType, NotNan};

    use crate::{
        expr::{BinaryExpr, GroupingExpr, LiteralExpr, LoxExpr, UnaryExpr},
        parser_from_str, LoxParser,
    };

    #[test]
    fn it_works() {
        let t: [LoxToken<'static>; 0] = [];
        let _ = LoxParser::new(t.into_iter());
        let _ = parser_from_str("").unwrap();
    }

    #[test]
    fn lex_simple_expr() {
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
}
