#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Normal,
    FirstSlash,
    InComment,
    EndOfFile,
}

#[derive(Debug)]
pub struct CommentRemover<I>
where
    I: Iterator<Item = (usize, char)>,
{
    inner: I,
    state: State,
    buffer: Option<(usize, char)>,
}

impl<I> CommentRemover<I>
where
    I: Iterator<Item = (usize, char)>,
{
    pub fn new(inner: I) -> Self {
        CommentRemover {
            inner,
            state: State::Normal,
            buffer: None,
        }
    }
}

impl<I> Iterator for CommentRemover<I>
where
    I: Iterator<Item = (usize, char)>,
{
    type Item = (usize, char);

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        if let Some(pair) = self.buffer.take() {
            return Some(pair);
        }

        while self.state != State::EndOfFile {
            let (next_state, pair) = match (self.state, self.inner.next()) {
                (_, None) => (State::EndOfFile, None),
                (State::EndOfFile, _) => unreachable!("Got character after a None"),
                (State::Normal, pair @ Some((_, '/'))) => {
                    self.buffer = pair;
                    (State::FirstSlash, None)
                }
                (State::Normal, pair) => (State::Normal, pair),
                (State::FirstSlash, Some((_, '/'))) => {
                    debug_assert!(self.buffer.is_some()); // buffer should be filled with previous slash
                    self.buffer = None;
                    (State::InComment, None)
                }
                (State::FirstSlash, pair) => {
                    debug_assert!(self.buffer.is_some()); // buffer should be filled with previous slash
                    let next_pair = self.buffer.take();
                    self.buffer = pair;
                    (State::Normal, next_pair)
                }
                (State::InComment, pair @ Some((_, '\n'))) => (State::Normal, pair),
                (State::InComment, _) => (State::InComment, None),
            };

            self.state = next_state;
            if pair.is_some() {
                return pair;
            }
        }

        None
    }
}
