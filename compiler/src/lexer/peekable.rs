use super::{Lexer, LexerError, SpannedToken};

#[derive(Debug, Clone)]
pub struct PeekableLexer<'c, I>
where
    I: Iterator<Item = (usize, char)>,
{
    inner: Lexer<'c, I>,
    peeked: Option<SpannedToken>,
}

impl<'c, I> PeekableLexer<'c, I>
where
    I: Iterator<Item = (usize, char)>,
{
    pub fn new(inner: Lexer<'c, I>) -> Self {
        PeekableLexer {
            inner,
            peeked: None,
        }
    }

    pub fn next_token(&mut self) -> Result<SpannedToken, LexerError> {
        if let Some(saved) = self.peeked.take() {
            Ok(saved)
        } else {
            self.inner.next_token()
        }
    }

    pub fn peek_token(&mut self) -> Result<SpannedToken, LexerError> {
        if let Some(saved) = self.peeked {
            Ok(saved)
        } else {
            let next = self.inner.next_token()?;
            self.peeked = Some(next);
            Ok(next)
        }
    }
}
