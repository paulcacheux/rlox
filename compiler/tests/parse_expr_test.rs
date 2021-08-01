use compiler::{
    ast::eval::{self, Evaluator},
    lexer::Lexer,
    parse_tree as pt,
    parser::Parser,
    pt2ast::Translator,
    tree_common as tc, CompilationContext,
};
use std::{
    borrow::Cow,
    io::{BufRead, BufReader},
};

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

fn extract_expect(path: &str) -> String {
    const EXPECT_PREFIX: &str = "// expect: ";

    let test_desc_file = std::fs::File::open(path).expect("Failed to open test description file");
    let reader = BufReader::new(test_desc_file);

    for line in reader.lines() {
        let line = line.expect("Failed to read line");

        if line.starts_with(EXPECT_PREFIX) {
            return line.trim_start_matches(EXPECT_PREFIX).to_owned();
        }
    }

    panic!("Failed to exctract expect from test description file")
}

fn parse_expression(context: &CompilationContext, path: &str) -> pt::Expression {
    let input_content = std::fs::read_to_string(path).expect("Failed to read input file");

    let lexer = Lexer::new(&context, &input_content);
    let mut parser = Parser::new(lexer.peekable());

    parser
        .parse_expression()
        .expect("Failed to parse expression")
}

#[test]
fn test_expression_parse_tree() {
    const INPUT_PATH: &str = "./../testsuite/expressions/parse.lox";
    let expected = extract_expect(INPUT_PATH);

    let context = CompilationContext::default();
    let expr = parse_expression(&context, INPUT_PATH);
    assert_eq!(expected, test_debug_expr(&context, &expr));
}

#[test]
fn test_expression_simple_eval() {
    const INPUT_PATH: &str = "./../testsuite/expressions/evaluate.lox";
    let expected = extract_expect(INPUT_PATH);

    let context = CompilationContext::default();
    let expr = parse_expression(&context, INPUT_PATH);

    let mut translator = Translator::new(&context);
    let expr = translator.translate_expression(expr);

    let mut evaluator = Evaluator::new(&context);
    let expr_value = evaluator
        .eval_expression(&expr)
        .expect("Failed to compute expression value");

    let expr_value_str: Cow<str> = match expr_value {
        eval::Value::Nil => "nil".into(),
        eval::Value::Number(value) => value.to_string().into(),
        eval::Value::Bool(value) => value.to_string().into(),
        eval::Value::String(sym) => context.resolve_str_symbol(sym).into(),
    };

    assert_eq!(expected, expr_value_str);
}
