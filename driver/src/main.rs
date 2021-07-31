use compiler::{
    lexer::{Lexer, Token},
    CompilationContext,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input_path = std::env::args().nth(1).expect("Failed to get input path");
    let content = std::fs::read_to_string(&input_path)?;

    let context = CompilationContext::default();
    let mut lexer = Lexer::new(&context, &content);

    loop {
        let st = lexer.next_token()?;
        println!("{:?}", st);
        if st.token == Token::EndOfFile {
            break;
        }
    }

    Ok(())
}
