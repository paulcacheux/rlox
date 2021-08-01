use crate::{lexer::Span, tree_common::IdentifierExpression};

use super::{Expression, Statement};

#[derive(Debug)]
pub enum Declaration {
    Var(VarDeclaration),
    Statement(Statement),
}

#[derive(Debug)]
pub struct VarDeclaration {
    pub var_keyword_span: Span,
    pub identifier: IdentifierExpression,
    pub init: Option<VarInit>,
    pub semicolon_span: Span,
}

#[derive(Debug)]
pub struct VarInit {
    pub expression: Box<Expression>,
    pub equal_span: Span,
}
