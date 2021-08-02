use compiler::{
    ast,
    lexer::{Lexer, Span},
    parse_tree as pt,
    parser::Parser,
    pt2ast::Translator,
    CompilationContext, ErrorSpannable,
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

pub fn extract_expect(test_desc_input: &str) -> ExpectInfo {
    const EXPECT_PREFIX: &str = "// expect: ";
    const EXPECT_RUNTIME_ERROR_PREFIX: &str = "// expect runtime error: ";

    let mut res = String::new();

    for line in test_desc_input.lines() {
        if let Some(pat_start) = line.find(EXPECT_PREFIX) {
            let rest_start = pat_start + EXPECT_PREFIX.len();
            res.push_str(&line[rest_start..]);
            res.push('\n');
        }

        if line.find(EXPECT_RUNTIME_ERROR_PREFIX).is_some() {
            return ExpectInfo::RuntimeError;
        }

        if let Some(captures) = EXPECT_COMPILE_ERROR.captures(&line) {
            let line = captures[1]
                .parse()
                .expect("Failed to parse compile error line");
            return ExpectInfo::CompileError { line };
        }
    }

    ExpectInfo::Output(res)
}

pub fn parse_expression(context: &CompilationContext, input_content: &str) -> pt::Expression {
    let lexer = Lexer::new(&context, &input_content);
    let mut parser = Parser::new(lexer.peekable());

    parser
        .parse_expression()
        .expect("Failed to parse expression")
}

pub fn parse_program(
    context: &CompilationContext,
    input_content: &str,
) -> Result<ast::Program, CompileError> {
    let lexer = Lexer::new(&context, &input_content);
    let mut parser = Parser::new(lexer.peekable());

    let program = parser.parse_program()?;

    let mut translator = Translator::new(&context);
    Ok(translator.translate_program(program)?)
}

pub struct CompileError {
    pub span: Span,
}

impl<E> From<E> for CompileError
where
    E: ErrorSpannable,
{
    fn from(err: E) -> Self {
        CompileError { span: err.span() }
    }
}

impl CompileError {
    pub fn get_line(&self, content: &str) -> usize {
        let mut line = 1;
        for (index, c) in content.char_indices() {
            if index >= self.span.begin {
                return line;
            }
            if c == '\n' {
                line += 1;
            }
        }
        line // Mostly EOF
    }
}
