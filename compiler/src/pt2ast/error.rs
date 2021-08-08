use std::fmt;

use crate::{lexer::Span, ErrorSpannable};

#[derive(Debug)]
pub enum SemanticError {
    IdentifierAlreadyDefined {
        identifier: String,
        identifier_span: Span,
    },
    LhsNotAssignable {
        lhs_span: Span,
    },
    ReturnOutsideFunction {
        return_span: Span,
    },
    CyclicDefinition {
        var_span: Span,
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
            SemanticError::LhsNotAssignable { lhs_span } => {
                write!(
                    f,
                    "[span: {:?}]: Left-hand side is not assignable",
                    lhs_span
                )
            }
            SemanticError::ReturnOutsideFunction { return_span } => {
                write!(f, "[span: {:?}]: Return outside of function", return_span)
            }
            SemanticError::CyclicDefinition { var_span } => {
                write!(f, "[span: {:?}]: Cyclic definition", var_span)
            }
        }
    }
}

impl std::error::Error for SemanticError {}

impl ErrorSpannable for SemanticError {
    fn span(&self) -> Span {
        match self {
            SemanticError::IdentifierAlreadyDefined {
                identifier_span, ..
            } => *identifier_span,
            SemanticError::LhsNotAssignable { lhs_span } => *lhs_span,
            SemanticError::ReturnOutsideFunction { return_span } => *return_span,
            SemanticError::CyclicDefinition { var_span } => *var_span,
        }
    }
}
