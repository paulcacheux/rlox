use crate::lexer::Span;
use crate::tree_common as tc;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
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
    Return {
        expression: Option<Box<Expression>>,
        return_keyword_span: Span,
        semicolon_span: Span,
    },
    Expression {
        expression: Option<Box<Expression>>,
        semicolon_span: Span,
    },
}

#[derive(Debug, Clone)]
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

    pub fn span(&self) -> Span {
        match self {
            Expression::AssignExpression(expr) => expr.span(),
            Expression::LazyLogical(expr) => expr.span(),
            Expression::Binary(expr) => expr.span(),
            Expression::Call(expr) => expr.span(),
            Expression::Unary(expr) => expr.span(),
            Expression::Literal(expr) => expr.span(),
            Expression::Identifier(expr) => expr.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AssignExpressionLhs {
    Identifier(tc::IdentifierExpression),
}

impl AssignExpressionLhs {
    pub fn span(&self) -> Span {
        match self {
            AssignExpressionLhs::Identifier(ident) => ident.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssignExpression {
    pub equal_span: Span,
    pub lhs: Box<AssignExpressionLhs>,
    pub rhs: Box<Expression>,
}

impl AssignExpression {
    pub fn span(&self) -> Span {
        Span::merge(self.lhs.span(), self.rhs.span())
    }
}

#[derive(Debug, Clone)]
pub struct LazyLogicalExpression {
    pub operator: LazyLogicalOperator,
    pub operator_span: Span,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

impl LazyLogicalExpression {
    pub fn span(&self) -> Span {
        Span::merge(self.lhs.span(), self.rhs.span())
    }
}

#[derive(Debug, Clone)]
pub enum LazyLogicalOperator {
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub operator_span: Span,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

impl BinaryExpression {
    pub fn span(&self) -> Span {
        Span::merge(self.lhs.span(), self.rhs.span())
    }
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

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: tc::UnaryOperator,
    pub operator_span: Span,
    pub sub: Box<Expression>,
}

impl UnaryExpression {
    pub fn span(&self) -> Span {
        Span::merge(self.operator_span, self.sub.span())
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub left_parenthesis_span: Span,
    pub right_parenthesis_span: Span,
}

impl CallExpression {
    pub fn span(&self) -> Span {
        Span::merge(self.function.span(), self.right_parenthesis_span)
    }
}
