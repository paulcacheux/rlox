use std::{
    io::{BufRead, BufReader},
    path::Path,
};

use compiler::{
    ast, lexer::Lexer, parse_tree as pt, parser::Parser, pt2ast::Translator, CompilationContext,
};
use once_cell::sync::Lazy;
use regex::Regex;

static EXPECT_COMPILE_ERROR: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"// \[line (\d+)\] Error:").expect("Failed to compile regex"));

#[macro_export]
macro_rules! extract_enum_value {
    ($value:expr, $pattern:pat => $extracted_value:expr) => {
        match $value {
            $pattern => $extracted_value,
            _ => panic!("Pattern doesn't match!"),
        }
    };
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpectInfo {
    Output(String),
    CompileError { line: usize },
    RuntimeError,
}

pub fn extract_expect<P: AsRef<Path>>(path: P) -> ExpectInfo {
    const EXPECT_PREFIX: &str = "// expect: ";
    const EXPECT_RUNTIME_ERROR_PREFIX: &str = "// expect runtime error: ";

    let test_desc_file = std::fs::File::open(path).expect("Failed to open test description file");
    let reader = BufReader::new(test_desc_file);

    let mut res = String::new();

    for line in reader.lines() {
        let line = line.expect("Failed to read line");

        if let Some(pat_start) = line.find(EXPECT_PREFIX) {
            let rest_start = pat_start + EXPECT_PREFIX.len();
            res.push_str(&line[rest_start..]);
            res.push('\n');
        }

        if line.find(EXPECT_RUNTIME_ERROR_PREFIX).is_some() {
            return ExpectInfo::RuntimeError;
        }

        if let Some(captures) = EXPECT_COMPILE_ERROR.captures(&line) {
            let _line: usize = captures[1]
                .parse()
                .expect("Failed to parse compile error line");
            return ExpectInfo::CompileError { line: 0 };
        }
    }

    ExpectInfo::Output(res)
}

pub fn parse_expression<P: AsRef<Path>>(context: &CompilationContext, path: P) -> pt::Expression {
    let input_content = std::fs::read_to_string(path).expect("Failed to read input file");

    let lexer = Lexer::new(&context, &input_content);
    let mut parser = Parser::new(lexer.peekable());

    parser
        .parse_expression()
        .expect("Failed to parse expression")
}

pub fn parse_program<P: AsRef<Path>>(
    context: &CompilationContext,
    path: P,
) -> Result<ast::Program, Box<dyn std::error::Error>> {
    let input_content = std::fs::read_to_string(path).expect("Failed to read input file");

    let lexer = Lexer::new(&context, &input_content);
    let mut parser = Parser::new(lexer.peekable());

    let program = parser.parse_program()?;

    let mut translator = Translator::new(&context);
    Ok(translator.translate_program(program)?)
}
