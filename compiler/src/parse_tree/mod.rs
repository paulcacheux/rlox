use string_interner::DefaultSymbol;

use crate::lexer::Span;

#[derive(Debug)]
pub enum Expression {
    Parenthesis(ParenthesisExpression),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Literal(LiteralExpression),
    Identifier(IdentifierExpression),
}

#[derive(Debug)]
pub struct ParenthesisExpression {
    pub left_paren_span: Span,
    pub right_paren_span: Span,
    sub: Box<Expression>,
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub operator_span: Span,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
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
    pub operator: UnaryOperator,
    pub operator_span: Span,
    pub sub: Box<Expression>,
}

#[derive(Debug)]
pub enum UnaryOperator {
    LogicalNot,
    Minus,
}

#[derive(Debug)]
pub struct LiteralExpression {
    pub literal: Literal,
    pub span: Span,
}

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(DefaultSymbol),
    Bool(bool),
    Nil,
}

#[derive(Debug)]
pub struct IdentifierExpression {
    pub identifier: DefaultSymbol,
    pub span: Span,
}
