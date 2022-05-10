use crate::expr::LoxExpr;

#[derive(Debug)]
pub enum LoxStmt<'a> {
    Expression(ExpressionStmt<'a>),
    Print(PrintStmt<'a>),
}

impl<'a> LoxStmt<'a> {
    pub fn accept<R>(&self, visitor: &mut dyn StmtVisitor<Output = R>) -> R {
        match self {
            LoxStmt::Expression(stmt) => visitor.visit_expression_stmt(stmt),
            LoxStmt::Print(stmt) => visitor.visit_print_stmt(stmt),
        }
    }
}

#[derive(Debug)]
pub struct ExpressionStmt<'a> {
    pub expr: Box<LoxExpr<'a>>,
}

impl<'a> ExpressionStmt<'a> {
    pub fn new(expr: impl Into<Box<LoxExpr<'a>>>) -> ExpressionStmt<'a> {
        ExpressionStmt { expr: expr.into() }
    }

    pub fn into_stmt(self) -> LoxStmt<'a> {
        LoxStmt::Expression(self)
    }
}

#[derive(Debug)]
pub struct PrintStmt<'a> {
    pub print_expr: Box<LoxExpr<'a>>,
}

impl<'a> PrintStmt<'a> {
    pub fn new(print_expr: impl Into<Box<LoxExpr<'a>>>) -> PrintStmt<'a> {
        PrintStmt {
            print_expr: print_expr.into(),
        }
    }

    pub fn into_stmt(self) -> LoxStmt<'a> {
        LoxStmt::Print(self)
    }
}

pub trait StmtVisitor {
    type Output;

    fn visit_expression_stmt(&mut self, stmt: &ExpressionStmt<'_>) -> Self::Output;
    fn visit_print_stmt(&mut self, stmt: &PrintStmt<'_>) -> Self::Output;
}
