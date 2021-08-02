use compiler::{
    ast_eval::Evaluator, lexer::Lexer, parser::Parser, pt2ast::Translator, CompilationContext,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input_path = std::env::args().nth(1).expect("Failed to get input path");
    let content = std::fs::read_to_string(&input_path)?;

    let context = CompilationContext::default();
    let lexer = Lexer::new(&context, &content);

    let mut parser = Parser::new(lexer.peekable());

    let program = parser.parse_program().expect("Failed to parse program");

    let mut translator = Translator::new(&context);
    let program = translator
        .translate_program(program)
        .expect("Failed to translate parse tree to AST");

    // println!("{:#?}", program);

    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();
    let mut evaluator = Evaluator::new(&context, &mut stdout);
    evaluator
        .eval_program(&program)
        .expect("Failed to eval program");

    Ok(())
}
