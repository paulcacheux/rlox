use crate::{lexer::Span, parse_tree as pt};

use super::VarDeclaration;

#[derive(Debug)]
pub enum Statement {
    Block(BlockStatement),
    Expression(ExpressionStatement),
    If(IfStatement),
    While(WhileStatement),
    For(ForStatement),
    Print(PrintStatement),
    Return(ReturnStatement),
}

#[derive(Debug)]
pub struct BlockStatement {
    pub declarations: Vec<pt::Declaration>,
    pub left_bracket_span: Span,
    pub right_bracket_span: Span,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Option<Box<pt::Expression>>,
    pub semicolon_span: Span,
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Box<pt::Expression>,
    pub if_keyword_span: Span,
    pub left_paren_span: Span,
    pub right_paren_span: Span,
    pub body: Box<Statement>,
    pub else_statement: Option<ElseStatement>,
}

#[derive(Debug)]
pub struct ElseStatement {
    pub else_keyword_span: Span,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct WhileStatement {
    pub condition: Box<pt::Expression>,
    pub while_keyword_span: Span,
    pub left_paren_span: Span,
    pub right_paren_span: Span,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct ForStatement {
    pub init: Box<VarDeclOrExpressionStatement>,
    pub condition: Box<pt::ExpressionStatement>,
    pub step: Option<Box<pt::Expression>>,
    pub for_keyword_span: Span,
    pub left_paren_span: Span,
    pub right_paren_span: Span,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub enum VarDeclOrExpressionStatement {
    Var(VarDeclaration),
    Expr(ExpressionStatement),
}

#[derive(Debug)]
pub struct PrintStatement {
    pub expression: Box<pt::Expression>,
    pub print_keyword_span: Span,
    pub semicolon_span: Span,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub expression: Option<Box<pt::Expression>>,
    pub return_keyword_span: Span,
    pub semicolon_span: Span,
}
