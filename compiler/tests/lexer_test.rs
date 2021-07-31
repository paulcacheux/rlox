use compiler::lexer::{Lexer, SpannedToken, Token};
use once_cell::sync::Lazy;
use regex;
use std::{
    io::{BufRead, BufReader},
    path::Path,
};
use test_generator::test_resources;

#[derive(Debug, PartialEq)]
struct LexerExpectToken {
    kind: String,
    literal: String,
    number_value: Option<f64>,
}

static EXPECT_REGEX: Lazy<regex::Regex> = Lazy::new(|| {
    regex::Regex::new(r"// expect: (\w+) (\S*) (\S+)").expect("Failed to compile regex")
});

impl LexerExpectToken {
    fn simple_kind(kind: &'static str, literal: String) -> Self {
        LexerExpectToken {
            kind: kind.to_owned(),
            literal,
            number_value: None,
        }
    }

    fn from_token(st: SpannedToken, input_source: &str) -> Self {
        let literal = if st.span.is_invalid() {
            String::new()
        } else {
            input_source[st.span.pos..st.span.pos + st.span.len].to_owned()
        };

        match st.token {
            Token::NumberLiteral(value) => LexerExpectToken {
                kind: "NUMBER".to_owned(),
                literal,
                number_value: Some(value),
            },
            Token::StringLiteral(_) => todo!(),
            Token::BoolLiteral(value) => LexerExpectToken {
                kind: if value { "TRUE" } else { "FALSE" }.to_owned(),
                literal,
                number_value: None,
            },
            Token::Identifier(id) => {
                assert_eq!(literal, id, "literal and identifier should match");
                LexerExpectToken {
                    kind: "IDENTIFIER".to_owned(),
                    literal,
                    number_value: None,
                }
            }
            Token::ClassKeyword => LexerExpectToken::simple_kind("CLASS", literal),
            Token::FunKeyword => LexerExpectToken::simple_kind("FUN", literal),
            Token::VarKeyword => LexerExpectToken::simple_kind("VAR", literal),
            Token::ForKeyword => LexerExpectToken::simple_kind("FOR", literal),
            Token::IfKeyword => LexerExpectToken::simple_kind("IF", literal),
            Token::ElseKeyword => LexerExpectToken::simple_kind("ELSE", literal),
            Token::WhileKeyword => LexerExpectToken::simple_kind("WHILE", literal),
            Token::PrintKeyword => LexerExpectToken::simple_kind("PRINT", literal),
            Token::ReturnKeyword => LexerExpectToken::simple_kind("RETURN", literal),
            Token::AndKeyword => LexerExpectToken::simple_kind("AND", literal),
            Token::OrKeyword => LexerExpectToken::simple_kind("OR", literal),
            Token::NilKeyword => LexerExpectToken::simple_kind("NIL", literal),
            Token::ThisKeyword => LexerExpectToken::simple_kind("THIS", literal),
            Token::SuperKeyword => LexerExpectToken::simple_kind("SUPER", literal),
            Token::LeftParenthesis => LexerExpectToken::simple_kind("LEFT_PAREN", literal),
            Token::RightParenthesis => LexerExpectToken::simple_kind("RIGHT_PAREN", literal),
            Token::LeftBracket => LexerExpectToken::simple_kind("LEFT_BRACE", literal),
            Token::RightBracket => LexerExpectToken::simple_kind("RIGHT_BRACE", literal),
            Token::SemiColon => LexerExpectToken::simple_kind("SEMICOLON", literal),
            Token::Dot => LexerExpectToken::simple_kind("DOT", literal),
            Token::Comma => LexerExpectToken::simple_kind("COMMA", literal),
            Token::LessThan => LexerExpectToken::simple_kind("LESS", literal),
            Token::LessOrEqual => LexerExpectToken::simple_kind("LESS_EQUAL", literal),
            Token::GreaterThan => LexerExpectToken::simple_kind("GREATER", literal),
            Token::GreaterOrEqual => LexerExpectToken::simple_kind("GREATER_EQUAL", literal),
            Token::Equal => LexerExpectToken::simple_kind("EQUAL", literal),
            Token::EqualEqual => LexerExpectToken::simple_kind("EQUAL_EQUAL", literal),
            Token::BangEqual => LexerExpectToken::simple_kind("BANG_EQUAL", literal),
            Token::Bang => LexerExpectToken::simple_kind("BANG", literal),
            Token::Minus => LexerExpectToken::simple_kind("MINUS", literal),
            Token::Plus => LexerExpectToken::simple_kind("PLUS", literal),
            Token::Slash => LexerExpectToken::simple_kind("SLASH", literal),
            Token::Star => LexerExpectToken::simple_kind("STAR", literal),
            Token::EndOfFile => LexerExpectToken::simple_kind("EOF", literal),
        }
    }

    fn parse_from_line(line: &str) -> Option<Self> {
        if let Some(captures) = EXPECT_REGEX.captures(&line) {
            let number_value_str = &captures[3];
            let number_value = if number_value_str == "null" {
                None
            } else {
                number_value_str.parse().ok()
            };

            Some(LexerExpectToken {
                kind: captures[1].to_owned(),
                literal: captures[2].to_owned(),
                number_value,
            })
        } else {
            None
        }
    }
}

#[test_resources("testsuite/scanning/*.lox")]
fn expected_tokens_test(input_path: &str) {
    let actual_input_path = Path::new("./../").join(input_path);

    let input_content =
        std::fs::read_to_string(&actual_input_path).expect("Failed to read input file");
    let mut lexer = Lexer::new(&input_content);

    let test_desc_file =
        std::fs::File::open(&actual_input_path).expect("Failed to open test description file");

    let reader = BufReader::new(test_desc_file);
    for line in reader.lines() {
        let line = line.expect("Failed to read line");

        if let Some(expect_token) = LexerExpectToken::parse_from_line(&line) {
            let given_token = lexer.next_token().expect("Failed to get next token");
            let given_token = LexerExpectToken::from_token(given_token, &input_content);

            assert_eq!(
                given_token, expect_token,
                "given and expected token should match"
            );
        }
    }
}
