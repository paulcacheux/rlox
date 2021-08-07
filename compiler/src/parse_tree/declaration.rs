use crate::{lexer::Span, tree_common as tc};

use super::{BlockStatement, Expression, Statement};

#[derive(Debug)]
pub enum Declaration {
    Var(VarDeclaration),
    Fun(FunctionDeclaration),
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

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub fun_keyword_span: Span,
    pub function_name: tc::IdentifierExpression,
    pub parameters: Vec<tc::IdentifierExpression>,
    pub left_parenthesis_span: Span,
    pub right_parenthesis_span: Span,
    pub body: Box<BlockStatement>,
}
