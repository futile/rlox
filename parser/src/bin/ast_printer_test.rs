use lexer::{LoxToken, LoxTokenType, NotNan};
use parser::{
    ast_printer::ast_display,
    expr::{BinaryExpr, GroupingExpr, LiteralExpr, LoxExpr, UnaryExpr},
};

pub fn main() {
    let expr = LoxExpr::Binary(BinaryExpr::new(
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

    println!("{}", ast_display(&expr));
}
