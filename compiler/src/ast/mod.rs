use crate::lexer::Span;
use crate::tree_common as tc;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    VarDeclaration {
        var_keyword_span: Span,
        identifier: tc::IdentifierExpression,
        init_expression: Box<Expression>,
        equal_span: Span,
        semicolon_span: Span,
    },
    FunctionDeclaration {
        fun_keyword_span: Span,
        function_name: tc::IdentifierExpression,
        parameters: Vec<tc::IdentifierExpression>,
        left_parenthesis_span: Span,
        right_parenthesis_span: Span,
        body: Box<Statement>,
    },
    Block {
        statements: Vec<Statement>,
        left_bracket_span: Span,
        right_bracket_span: Span,
    },
    If {
        condition: Box<Expression>,
        if_keyword_span: Span,
        left_paren_span: Span,
        right_paren_span: Span,
        true_body: Box<Statement>,
        else_keyword_span: Span,
        false_body: Box<Statement>,
    },
    While {
        condition: Box<Expression>,
        while_keyword_span: Span,
        left_paren_span: Span,
        right_paren_span: Span,
        body: Box<Statement>,
    },
    Print {
        expression: Box<Expression>,
        print_keyword_span: Span,
        semicolon_span: Span,
    },
    Expression {
        expression: Option<Box<Expression>>,
        semicolon_span: Span,
    },
}

#[derive(Debug)]
pub enum Expression {
    AssignExpression(AssignExpression),
    LazyLogical(LazyLogicalExpression),
    Binary(BinaryExpression),
    Call(CallExpression),
    Unary(UnaryExpression),
    Literal(tc::LiteralExpression),
    Identifier(tc::IdentifierExpression),
}

impl Expression {
    pub fn into_assign(self) -> Option<AssignExpressionLhs> {
        if let Expression::Identifier(ident) = self {
            Some(AssignExpressionLhs::Identifier(ident))
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub enum AssignExpressionLhs {
    Identifier(tc::IdentifierExpression),
}

#[derive(Debug)]
pub struct AssignExpression {
    pub equal_span: Span,
    pub lhs: Box<AssignExpressionLhs>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct LazyLogicalExpression {
    pub operator: LazyLogicalOperator,
    pub operator_span: Span,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub enum LazyLogicalOperator {
    LogicalAnd,
    LogicalOr,
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

#[derive(Debug)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub left_parenthesis_span: Span,
    pub right_parenthesis_span: Span,
}
