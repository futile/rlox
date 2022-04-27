use lexer::LoxToken;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoxExpr<'a> {
    Binary(BinaryExpr<'a>),
    Unary(UnaryExpr<'a>),
    Grouping(GroupingExpr<'a>),
    Literal(LiteralExpr<'a>),
}

impl<'a> LoxExpr<'a> {
    pub fn accept<R>(&self, visitor: &mut dyn ExprVisitor<Output = R>) -> R {
        match self {
            LoxExpr::Binary(expr) => visitor.visit_binary_expr(expr),
            LoxExpr::Unary(expr) => visitor.visit_unary_expr(expr),
            LoxExpr::Grouping(expr) => visitor.visit_grouping_expr(expr),
            LoxExpr::Literal(expr) => visitor.visit_literal_expr(expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr<'a> {
    pub left: Box<LoxExpr<'a>>,
    pub operator: LoxToken<'a>,
    pub right: Box<LoxExpr<'a>>,
}

impl<'a> BinaryExpr<'a> {
    pub fn new(
        left: impl Into<Box<LoxExpr<'a>>>,
        operator: LoxToken<'a>,
        right: impl Into<Box<LoxExpr<'a>>>,
    ) -> BinaryExpr<'a> {
        BinaryExpr {
            left: left.into(),
            operator,
            right: right.into(),
        }
    }

    pub fn into_expr(self) -> LoxExpr<'a> {
        LoxExpr::Binary(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryExpr<'a> {
    pub operator: LoxToken<'a>,
    pub right: Box<LoxExpr<'a>>,
}

impl<'a> UnaryExpr<'a> {
    pub fn new(operator: LoxToken<'a>, right: impl Into<Box<LoxExpr<'a>>>) -> UnaryExpr<'a> {
        UnaryExpr {
            operator,
            right: right.into(),
        }
    }

    pub fn into_expr(self) -> LoxExpr<'a> {
        LoxExpr::Unary(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GroupingExpr<'a> {
    pub inner: Box<LoxExpr<'a>>,
}

impl<'a> GroupingExpr<'a> {
    pub fn new(inner: impl Into<Box<LoxExpr<'a>>>) -> GroupingExpr<'a> {
        GroupingExpr {
            inner: inner.into(),
        }
    }

    pub fn into_expr(self) -> LoxExpr<'a> {
        LoxExpr::Grouping(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LiteralExpr<'a> {
    pub literal: LoxToken<'a>,
}

impl<'a> LiteralExpr<'a> {
    pub fn new(literal: LoxToken<'a>) -> LiteralExpr<'a> {
        LiteralExpr { literal }
    }

    pub fn into_expr(self) -> LoxExpr<'a> {
        LoxExpr::Literal(self)
    }
}

pub trait ExprVisitor {
    type Output;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr<'_>) -> Self::Output;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr<'_>) -> Self::Output;
    fn visit_grouping_expr(&mut self, expr: &GroupingExpr<'_>) -> Self::Output;
    fn visit_literal_expr(&mut self, expr: &LiteralExpr<'_>) -> Self::Output;
}
