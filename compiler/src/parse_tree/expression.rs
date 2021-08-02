use crate::lexer::Span;
use crate::tree_common as tc;

#[derive(Debug)]
pub enum Expression {
    Assignment(AssignmentExpression),
    Parenthesis(ParenthesisExpression),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Literal(tc::LiteralExpression),
    Identifier(tc::IdentifierExpression),
}

#[derive(Debug)]
pub struct AssignmentExpression {
    pub equal_span: Span,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct ParenthesisExpression {
    pub left_paren_span: Span,
    pub right_paren_span: Span,
    pub sub: Box<Expression>,
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
    LogicalAnd,
    LogicalOr,
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
