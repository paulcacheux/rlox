use crate::{lexer::Span, parse_tree as pt};

#[derive(Debug)]
pub enum Statement {
    Block(BlockStatement),
    Expression(ExpressionStatement),
    If(IfStatement),
    Print(PrintStatement),
}

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
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
pub struct PrintStatement {
    pub expression: Box<pt::Expression>,
    pub print_keyword_span: Span,
    pub semicolon_span: Span,
}
