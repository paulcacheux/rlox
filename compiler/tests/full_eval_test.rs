use compiler::{ast_eval::Evaluator, CompilationContext};
use std::{io::Cursor, path::Path};
use test_generator::test_resources;

use crate::utils::ExpectInfo;

pub mod utils;

fn inner_eval_test(input_path: &str) {
    let actual_input_path = Path::new("./../").join(input_path);

    let content = std::fs::read_to_string(&actual_input_path).expect("Failed to read input file");
    let expected = utils::extract_expect(&content);

    let context = CompilationContext::default();
    let program = utils::parse_program(&context, &content);
    match program {
        Ok(program) => {
            let mut stdout: Cursor<Vec<u8>> = Cursor::new(Vec::new());
            let mut evaluator = Evaluator::new(&context, &mut stdout);
            if evaluator.eval_program(&program).is_ok() {
                let result =
                    String::from_utf8(stdout.into_inner()).expect("Failed to parse utf-8 output");

                assert_eq!(expected, ExpectInfo::Output(result));
            } else {
                assert_eq!(expected, ExpectInfo::RuntimeError)
            }
        }
        Err(err) => {
            assert_eq!(
                expected,
                ExpectInfo::CompileError {
                    line: err.get_line(&content)
                }
            );
        }
    }
}

#[test_resources("testsuite/assignment/*.lox")]
fn test_expression_full_eval_assignment(input_path: &str) {
    if input_path.contains("this") || input_path.contains("grouping") {
        return; // skip unimplemented or "not sure" stuff
    }
    inner_eval_test(input_path)
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

#[test_resources("testsuite/if/*.lox")]
fn test_expression_full_eval_if(input_path: &str) {
    if input_path.contains("class") {
        return; // skip unimplemented stuff
    }
    inner_eval_test(input_path)
}

#[test_resources("testsuite/while/syntax.lox")]
fn test_expression_full_eval_while(input_path: &str) {
    inner_eval_test(input_path)
}

#[test_resources("testsuite/for/*.lox")]
fn test_expression_full_eval_for(input_path: &str) {
    if input_path.contains("statement_") || input_path.contains("var_in_body") {
        inner_eval_test(input_path)
    }
}

#[test_resources("testsuite/nil/*.lox")]
fn test_expression_full_eval_nil(input_path: &str) {
    inner_eval_test(input_path)
}

#[test_resources("testsuite/string/*.lox")]
fn test_expression_full_eval_string(input_path: &str) {
    inner_eval_test(input_path)
}

#[test_resources("testsuite/function/*.lox")]
fn test_expression_full_eval_function(input_path: &str) {
    inner_eval_test(input_path)
}
