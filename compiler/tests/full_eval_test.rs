use std::io::Cursor;

use compiler::{ast_eval::Evaluator, CompilationContext};

pub mod utils;

#[test]
fn test_expression_full_eval() {
    const INPUT_PATH: &str = "./../testsuite/block/scope.lox";
    let expected = utils::extract_expect(INPUT_PATH);

    let context = CompilationContext::default();
    let program = utils::parse_program(&context, INPUT_PATH);

    let mut stdout: Cursor<Vec<u8>> = Cursor::new(Vec::new());
    let mut evaluator = Evaluator::new(&context, &mut stdout);
    evaluator
        .eval_program(&program)
        .expect("Failed to evaluate program");

    let result = String::from_utf8(stdout.into_inner()).expect("Failed to parse utf-8 output");

    assert_eq!(expected, result);
}
