use compiler::{ast_eval::Evaluator, CompilationContext};
use std::{io::Cursor, path::Path};
use test_generator::test_resources;

pub mod utils;

fn inner_eval_test(input_path: &str) {
    let actual_input_path = Path::new("./../").join(input_path);

    let expected = utils::extract_expect(&actual_input_path);

    let context = CompilationContext::default();
    let program = utils::parse_program(&context, &actual_input_path);

    let mut stdout: Cursor<Vec<u8>> = Cursor::new(Vec::new());
    let mut evaluator = Evaluator::new(&context, &mut stdout);
    evaluator
        .eval_program(&program)
        .expect("Failed to evaluate program");

    let result = String::from_utf8(stdout.into_inner()).expect("Failed to parse utf-8 output");

    assert_eq!(expected, result);
}

#[test_resources("testsuite/block/*.lox")]
fn test_expression_full_eval_block(input_path: &str) {
    inner_eval_test(input_path)
}

#[test_resources("testsuite/bool/*.lox")]
fn test_expression_full_eval_bool(input_path: &str) {
    inner_eval_test(input_path)
}

#[test_resources("testsuite/comments/*.lox")]
fn test_expression_full_eval_comments(input_path: &str) {
    inner_eval_test(input_path)
}

#[test_resources("testsuite/nil/*.lox")]
fn test_expression_full_eval_nil(input_path: &str) {
    inner_eval_test(input_path)
}
