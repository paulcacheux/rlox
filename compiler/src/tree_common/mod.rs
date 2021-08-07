use string_interner::DefaultSymbol;

use crate::lexer::Span;

#[derive(Debug, Clone)]
pub struct LiteralExpression {
    pub literal: Literal,
    pub span: Span,
}

impl LiteralExpression {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(DefaultSymbol),
    Bool(bool),
    Nil,
}

#[derive(Debug, Clone, Copy)]
pub struct IdentifierExpression {
    pub identifier: DefaultSymbol,
    pub span: Span,
}

impl IdentifierExpression {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    LogicalNot,
    Minus,
}
