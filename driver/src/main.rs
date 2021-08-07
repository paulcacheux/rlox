use clap::Clap;
use compiler::{
    ast_eval::Evaluator, lexer::Lexer, parser::Parser, pt2ast::Translator, CompilationContext,
};

/// rlox, clox interpreter in Rust
#[derive(Clap)]
#[clap(
    name = "rlox_driver",
    version = "1.0",
    author = "Paul C. <paulcacheux@gmail.com>"
)]
struct Options {
    /// Source code input path
    input_path: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opts = Options::parse();
    let content = std::fs::read_to_string(&opts.input_path)?;

    let context = CompilationContext::default();
    let lexer = Lexer::new(&context, &content);

    let mut parser = Parser::new(lexer.peekable());

    let program = parser.parse_program()?;

    let mut translator = Translator::new(&context);
    let program = translator.translate_program(program)?;

    // println!("{:#?}", program);

    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();
    let mut evaluator = Evaluator::new(&context, &mut stdout);
    evaluator.eval_program(&program)?;

    Ok(())
}
