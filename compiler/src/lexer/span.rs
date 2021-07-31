use super::Token;

#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

impl SpannedToken {
    pub fn single_char(token: Token, pos: usize) -> Self {
        SpannedToken {
            token,
            span: Span { pos, len: 1 },
        }
    }

    pub fn eof() -> Self {
        SpannedToken {
            token: Token::EndOfFile,
            span: Span::INVALID,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub pos: usize,
    pub len: usize,
}

impl Span {
    const INVALID: Self = Span {
        pos: usize::MAX,
        len: 0,
    };

    pub fn is_invalid(&self) -> bool {
        *self == Self::INVALID
    }
}
