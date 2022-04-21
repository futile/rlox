use std::fmt::{Display, Formatter};

use crate::expr::{BinaryExpr, ExprVisitor, GroupingExpr, LiteralExpr, LoxExpr, UnaryExpr};

pub fn ast_display(expr: LoxExpr<'_>) -> impl Display + '_ {
    AstDisplay { expr }
}

struct AstDisplay<'a> {
    expr: LoxExpr<'a>,
}

impl<'a> Display for AstDisplay<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.expr.accept(&mut AstPrinter { writer: f })
    }
}

struct AstPrinter<'b, 'c> {
    writer: &'b mut Formatter<'c>,
}

impl<'b, 'c> ExprVisitor for AstPrinter<'b, 'c> {
    type Output = std::fmt::Result;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr<'_>) -> Self::Output {
        write!(self.writer, "({} ", expr.operator.lexeme)?;
        expr.left.accept(self)?;
        write!(self.writer, " ")?;
        expr.right.accept(self)?;
        write!(self.writer, ")")
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr<'_>) -> Self::Output {
        write!(self.writer, "({} ", expr.operator.lexeme)?;
        expr.right.accept(self)?;
        write!(self.writer, ")")
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr<'_>) -> Self::Output {
        write!(self.writer, "(group ")?;
        expr.inner.accept(self)?;
        write!(self.writer, ")")
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr<'_>) -> Self::Output {
        write!(self.writer, "{}", expr.literal.lexeme)
    }
}
