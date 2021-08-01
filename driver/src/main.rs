use compiler::{
    ast::{
        self,
        eval::{self, Evaluator},
    },
    lexer::Lexer,
    parser::Parser,
    CompilationContext,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input_path = std::env::args().nth(1).expect("Failed to get input path");
    let content = std::fs::read_to_string(&input_path)?;

    let context = CompilationContext::default();
    let lexer = Lexer::new(&context, &content);

    let mut parser = Parser::new(lexer.peekable());

    let expr = parser
        .parse_expression()
        .expect("Failed to parse expression");

    let ast_expr = ast::translate::build_ast_expression(expr);

    let mut evaluator = Evaluator::new(&context);
    let res = evaluator.eval_expression(&ast_expr)?;

    println!("{:#?}", ast_expr);
    println!("{:#?}", res);

    Ok(())
}
