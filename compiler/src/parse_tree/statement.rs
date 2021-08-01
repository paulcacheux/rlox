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
    statements: Vec<Statement>,
    left_bracket_span: Span,
    right_bracket_span: Span,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    expression: Box<pt::Expression>,
    semicolon_span: Span,
}

#[derive(Debug)]
pub struct IfStatement {
    condition: Box<pt::Expression>,
    if_keyword_span: Span,
    left_paren_span: Span,
    right_paren_span: Span,
    body: Box<Statement>,
    else_statement: Option<ElseStatement>,
}

#[derive(Debug)]
pub struct ElseStatement {
    else_keyword_span: Span,
    body: Box<Statement>,
}

#[derive(Debug)]
pub struct PrintStatement {
    expression: Box<pt::Expression>,
    print_keyword_span: Span,
    semicolon_span: Span,
}
