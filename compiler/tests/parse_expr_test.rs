use compiler::{
    lexer::Lexer, parse_tree as pt, parser::Parser, tree_common as tc, CompilationContext,
};
use std::io::{BufRead, BufReader};

fn test_debug_binary_operator(op: &pt::BinaryOperator) -> &'static str {
    match op {
        pt::BinaryOperator::LogicalAnd => "and",
        pt::BinaryOperator::LogicalOr => "or",
        pt::BinaryOperator::NotEqual => "!=",
        pt::BinaryOperator::Equal => "==",
        pt::BinaryOperator::LessThan => "<",
        pt::BinaryOperator::LessOrEqual => "<=",
        pt::BinaryOperator::GreaterThan => ">",
        pt::BinaryOperator::GreaterOrEqual => ">=",
        pt::BinaryOperator::Add => "+",
        pt::BinaryOperator::Substract => "-",
        pt::BinaryOperator::Multiply => "*",
        pt::BinaryOperator::Divide => "/",
    }
}

fn test_debug_unary_operator(op: &tc::UnaryOperator) -> &'static str {
    match op {
        tc::UnaryOperator::LogicalNot => "!",
        tc::UnaryOperator::Minus => "-",
    }
}

fn test_debug_expr(ctx: &CompilationContext, expr: &pt::Expression) -> String {
    match expr {
        pt::Expression::Parenthesis(pt::ParenthesisExpression { sub, .. }) => {
            format!("(group {})", test_debug_expr(ctx, sub))
        }
        pt::Expression::Binary(pt::BinaryExpression {
            operator, lhs, rhs, ..
        }) => format!(
            "({} {} {})",
            test_debug_binary_operator(operator),
            test_debug_expr(ctx, lhs),
            test_debug_expr(ctx, rhs)
        ),
        pt::Expression::Unary(pt::UnaryExpression { operator, sub, .. }) => format!(
            "({} {})",
            test_debug_unary_operator(operator),
            test_debug_expr(ctx, sub)
        ),
        pt::Expression::Literal(literal) => match literal.literal {
            tc::Literal::Number(value) => format!("{:.1}", value),
            tc::Literal::String(sym) => ctx.resolve_str_symbol(sym).escape_debug().to_string(),
            tc::Literal::Bool(value) => value.to_string(),
            tc::Literal::Nil => "nil".to_owned(),
        },
        pt::Expression::Identifier(ident) => ctx.resolve_str_symbol(ident.identifier),
    }
}

#[test]
fn test_expression_parse_tree() {
    const INPUT_PATH: &str = "./../testsuite/expressions/parse.lox";
    const EXPECT_PREFIX: &str = "// expect: ";

    let input_content = std::fs::read_to_string(INPUT_PATH).expect("Failed to read input file");

    let context = CompilationContext::default();
    let lexer = Lexer::new(&context, &input_content);
    let mut parser = Parser::new(lexer.peekable());

    let expr = parser
        .parse_expression()
        .expect("Failed to parse expression");

    let test_desc_file =
        std::fs::File::open(INPUT_PATH).expect("Failed to open test description file");

    let reader = BufReader::new(test_desc_file);
    let mut expected = None;

    for line in reader.lines() {
        let line = line.expect("Failed to read line");

        if line.starts_with(EXPECT_PREFIX) {
            expected = Some(line.trim_start_matches(EXPECT_PREFIX).to_owned());
        }
    }

    let expected = expected.expect("Failed to read expect line from description file");

    assert_eq!(expected, test_debug_expr(&context, &expr));
}
