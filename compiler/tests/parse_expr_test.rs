use compiler::{
    ast_eval::Evaluator, parse_tree as pt, pt2ast::Translator, tree_common as tc,
    CompilationContext,
};

pub mod utils;

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
        _ => unimplemented!(),
    }
}

#[test]
fn test_expression_parse_tree() {
    const INPUT_PATH: &str = "./../testsuite/expressions/parse.lox";
    let input_content =
        std::fs::read_to_string(INPUT_PATH).expect("Failed to read test input file content");

    let expected = extract_enum_value!(utils::extract_expect(&input_content), utils::ExpectInfo::Output(output) => output);

    let context = CompilationContext::default();
    let expr = utils::parse_expression(&context, &input_content);
    assert_eq!(expected, test_debug_expr(&context, &expr) + "\n");
}

#[test]
fn test_expression_simple_eval() {
    const INPUT_PATH: &str = "./../testsuite/expressions/evaluate.lox";
    let input_content =
        std::fs::read_to_string(INPUT_PATH).expect("Failed to read test input file content");

    let expected = extract_enum_value!(utils::extract_expect(&input_content), utils::ExpectInfo::Output(output) => output);

    let context = CompilationContext::default();
    let expr = utils::parse_expression(&context, &&input_content);

    let mut translator = Translator::new(&context);
    let expr = translator
        .translate_expression(expr)
        .expect("Failed to translate expression from parse tree to ast");

    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();
    let mut evaluator = Evaluator::new(&context, &mut stdout);
    let expr_value = evaluator
        .eval_expression(&expr)
        .expect("Failed to compute expression value");

    let expr_value_str = evaluator.value_to_str(expr_value) + "\n";

    assert_eq!(expected, expr_value_str);
}
