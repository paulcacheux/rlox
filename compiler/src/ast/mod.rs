use crate::lexer::Span;
use crate::tree_common as tc;

pub mod eval;
pub mod translate;

#[derive(Debug)]
pub enum Expression {
    LazyLogical(LazyLogicalExpression),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Literal(tc::LiteralExpression),
    Identifier(tc::IdentifierExpression),
}

#[derive(Debug)]
pub struct LazyLogicalExpression {
    pub operator: LazyLogicalOperator,
    pub operator_span: Span,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub enum LazyLogicalOperator {
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub operator_span: Span,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    NotEqual,
    Equal,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Add,
    Substract,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub struct UnaryExpression {
    pub operator: tc::UnaryOperator,
    pub operator_span: Span,
    pub sub: Box<Expression>,
}
