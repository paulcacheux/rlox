use std::{borrow::Cow, fmt};

use crate::lexer::Span;

#[derive(Debug)]
pub struct EvalError {
    pub msg: Cow<'static, str>,
    pub span: Span,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[span: {:?}] {}", self.span, self.msg)
    }
}

impl std::error::Error for EvalError {}
