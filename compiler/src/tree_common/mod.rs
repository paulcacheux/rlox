use string_interner::DefaultSymbol;

use crate::lexer::Span;

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

#[derive(Debug)]
pub enum UnaryOperator {
    LogicalNot,
    Minus,
}
