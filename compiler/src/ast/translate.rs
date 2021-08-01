use crate::ast;
use crate::parse_tree as pt;

pub fn build_ast_expression(expr: pt::Expression) -> ast::Expression {
    match expr {
        pt::Expression::Parenthesis(pt::ParenthesisExpression { sub, .. }) => {
            build_ast_expression(*sub)
        }
        pt::Expression::Binary(bin_expr) => build_ast_binary_expression(bin_expr),
        pt::Expression::Unary(pt::UnaryExpression {
            operator,
            operator_span,
            sub,
        }) => ast::Expression::Unary(ast::UnaryExpression {
            operator,
            operator_span,
            sub: Box::new(build_ast_expression(*sub)),
        }),
        pt::Expression::Literal(literal) => ast::Expression::Literal(literal),
        pt::Expression::Identifier(id) => ast::Expression::Identifier(id),
    }
}

fn build_ast_binary_expression(expr: pt::BinaryExpression) -> ast::Expression {
    let pt::BinaryExpression {
        operator,
        operator_span,
        lhs,
        rhs,
    } = expr;

    enum LogicalOrBinary {
        Logical(ast::LazyLogicalOperator),
        Binary(ast::BinaryOperator),
    }

    let lhs = Box::new(build_ast_expression(*lhs));
    let rhs = Box::new(build_ast_expression(*rhs));
    let op = match operator {
        pt::BinaryOperator::LogicalAnd => {
            LogicalOrBinary::Logical(ast::LazyLogicalOperator::LogicalAnd)
        }
        pt::BinaryOperator::LogicalOr => {
            LogicalOrBinary::Logical(ast::LazyLogicalOperator::LogicalOr)
        }
        pt::BinaryOperator::NotEqual => LogicalOrBinary::Binary(ast::BinaryOperator::NotEqual),
        pt::BinaryOperator::Equal => LogicalOrBinary::Binary(ast::BinaryOperator::Equal),
        pt::BinaryOperator::LessThan => LogicalOrBinary::Binary(ast::BinaryOperator::LessThan),
        pt::BinaryOperator::LessOrEqual => {
            LogicalOrBinary::Binary(ast::BinaryOperator::LessOrEqual)
        }
        pt::BinaryOperator::GreaterThan => {
            LogicalOrBinary::Binary(ast::BinaryOperator::GreaterThan)
        }
        pt::BinaryOperator::GreaterOrEqual => {
            LogicalOrBinary::Binary(ast::BinaryOperator::GreaterOrEqual)
        }
        pt::BinaryOperator::Add => LogicalOrBinary::Binary(ast::BinaryOperator::Add),
        pt::BinaryOperator::Substract => LogicalOrBinary::Binary(ast::BinaryOperator::Substract),
        pt::BinaryOperator::Multiply => LogicalOrBinary::Binary(ast::BinaryOperator::Multiply),
        pt::BinaryOperator::Divide => LogicalOrBinary::Binary(ast::BinaryOperator::Divide),
    };

    match op {
        LogicalOrBinary::Binary(op) => ast::Expression::Binary(ast::BinaryExpression {
            operator: op,
            operator_span,
            lhs,
            rhs,
        }),
        LogicalOrBinary::Logical(op) => ast::Expression::LazyLogical(ast::LazyLogicalExpression {
            operator: op,
            operator_span,
            lhs,
            rhs,
        }),
    }
}
