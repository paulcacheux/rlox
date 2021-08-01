use std::fmt;

use crate::lexer::Span;

#[derive(Debug)]
pub enum SemanticError {
    IdentifierAlreadyDefined {
        identifier: String,
        identifier_span: Span,
    },
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemanticError::IdentifierAlreadyDefined {
                identifier,
                identifier_span,
            } => write!(
                f,
                "[span: {:?}]: Identifier already defined `{}`",
                identifier_span, identifier
            ),
        }
    }
}

impl std::error::Error for SemanticError {}
