use std::fmt;

use crate::lexer::{LexerError, SpannedToken, Token};

#[derive(Debug)]
pub enum ParseError {
    LexerError(LexerError),
    UnexpectedToken {
        expected: Token,
        got: SpannedToken,
    },
    UnexpectedSyntax {
        msg: &'static str,
        got: SpannedToken,
    },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::LexerError(err) => write!(f, "{}", err),
            ParseError::UnexpectedToken { expected, got } => write!(
                f,
                "[span: {:?}] Unexpected token: `{:?}`. Expected `{:?}`",
                got.span, got.token, expected
            ),
            ParseError::UnexpectedSyntax { msg, got } => write!(
                f,
                "[span: {:?}] Unexpected token: `{:?}`. {}",
                got.span, got.token, msg
            ),
        }
    }
}

impl std::error::Error for ParseError {}

impl From<LexerError> for ParseError {
    fn from(err: LexerError) -> Self {
        ParseError::LexerError(err)
    }
}
