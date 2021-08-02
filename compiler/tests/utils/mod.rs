use std::io::{BufRead, BufReader};

use compiler::{
    ast, lexer::Lexer, parse_tree as pt, parser::Parser, pt2ast::Translator, CompilationContext,
};

pub fn extract_expect(path: &str) -> String {
    const EXPECT_PREFIX: &str = "// expect: ";

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
    }

    res
}

pub fn parse_expression(context: &CompilationContext, path: &str) -> pt::Expression {
    let input_content = std::fs::read_to_string(path).expect("Failed to read input file");

    let lexer = Lexer::new(&context, &input_content);
    let mut parser = Parser::new(lexer.peekable());

    parser
        .parse_expression()
        .expect("Failed to parse expression")
}

pub fn parse_program(context: &CompilationContext, path: &str) -> ast::Program {
    let input_content = std::fs::read_to_string(path).expect("Failed to read input file");

    let lexer = Lexer::new(&context, &input_content);
    let mut parser = Parser::new(lexer.peekable());

    let program = parser.parse_program().expect("Failed to parse program");

    let mut translator = Translator::new(&context);
    translator
        .translate_program(program)
        .expect("Failed to translate program")
}
