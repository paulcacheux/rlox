use crate::{lexer::Span, tree_common as tc};

use super::{Expression, Statement};

#[derive(Debug)]
pub enum Declaration {
    Var(VarDeclaration),
    Statement(Statement),
}

#[derive(Debug)]
pub struct VarDeclaration {
    pub var_keyword_span: Span,
    pub identifier: tc::IdentifierExpression,
    pub init: Option<VarInit>,
    pub semicolon_span: Span,
}

#[derive(Debug)]
pub struct VarInit {
    pub expression: Box<Expression>,
    pub equal_span: Span,
}
