use super::Token;

#[derive(Debug, Clone, Copy)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

impl SpannedToken {
    pub fn single_char(token: Token, pos: usize) -> Self {
        SpannedToken {
            token,
            span: Span {
                begin: pos,
                end: pos + 1,
            },
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
    pub begin: usize,
    pub end: usize,
}

impl Span {
    const INVALID: Self = Span {
        begin: usize::MAX,
        end: 0,
    };

    pub fn is_invalid(&self) -> bool {
        *self == Self::INVALID
    }
}
