mod comment_remover;
mod token;

use core::fmt;
use std::{iter::Peekable, str::CharIndices};

pub use comment_remover::CommentRemover;
pub use token::Token;

pub struct Lexer<I>
where
    I: Iterator<Item = (usize, char)>,
{
    src: Peekable<I>,
}

impl<'s> Lexer<CommentRemover<CharIndices<'s>>> {
    pub fn new(content: &'s str) -> Self {
        Lexer {
            src: CommentRemover::new(content.char_indices()).peekable(),
        }
    }
}

impl<I> Lexer<I>
where
    I: Iterator<Item = (usize, char)>,
{
    fn next_skip_whitespaces(&mut self) -> Option<(usize, char)> {
        while let Some((pos, c)) = self.src.next() {
            if c.is_ascii_whitespace() {
                continue;
            } else {
                return Some((pos, c));
            }
        }
        None
    }

    fn emit_if_next(
        &mut self,
        condition: char,
        if_true: Token,
        if_false: Token,
        start_pos: usize,
    ) -> SpannedToken {
        match self.src.peek() {
            Some(&(_, next_c)) if next_c == condition => {
                self.src.next();
                SpannedToken {
                    token: if_true,
                    span: Span {
                        pos: start_pos,
                        len: 2,
                    },
                }
            }
            _ => SpannedToken::single_char(if_false, start_pos),
        }
    }

    fn emit_identifier(&mut self, pos: usize, first_c: char) -> SpannedToken {
        let mut identifier = String::new();
        identifier.push(first_c);
        let mut len = 1;

        while let Some(&(_, c)) = self.src.peek() {
            if is_identifier_continue(c) {
                self.src.next();
                identifier.push(c);
                len += 1;
            } else {
                break;
            }
        }

        let token = match identifier.as_str() {
            "class" => Token::ClassKeyword,
            "fun" => Token::FunKeyword,
            "var" => Token::VarKeyword,
            "for" => Token::ForKeyword,
            "if" => Token::IfKeyword,
            "else" => Token::ElseKeyword,
            "while" => Token::WhileKeyword,
            "print" => Token::PrintKeyword,
            "return" => Token::ReturnKeyword,
            "and" => Token::AndKeyword,
            "or" => Token::OrKeyword,
            "nil" => Token::NilKeyword,
            "this" => Token::ThisKeyword,
            "super" => Token::SuperKeyword,
            "true" => Token::BoolLiteral(true),
            "false" => Token::BoolLiteral(false),
            _ => Token::Identifier(identifier),
        };

        SpannedToken {
            token,
            span: Span { pos, len },
        }
    }

    pub fn next_token(&mut self) -> Result<SpannedToken, LexerError> {
        let st = match self.next_skip_whitespaces() {
            Some((pos, '(')) => SpannedToken::single_char(Token::LeftParenthesis, pos),
            Some((pos, ')')) => SpannedToken::single_char(Token::RightParenthesis, pos),
            Some((pos, '{')) => SpannedToken::single_char(Token::LeftBracket, pos),
            Some((pos, '}')) => SpannedToken::single_char(Token::RightBracket, pos),
            Some((pos, ',')) => SpannedToken::single_char(Token::Comma, pos),
            Some((pos, ';')) => SpannedToken::single_char(Token::SemiColon, pos),
            Some((pos, '.')) => SpannedToken::single_char(Token::Dot, pos),
            Some((pos, '/')) => SpannedToken::single_char(Token::Slash, pos),
            Some((pos, '*')) => SpannedToken::single_char(Token::Star, pos),
            Some((pos, '+')) => SpannedToken::single_char(Token::Plus, pos),
            Some((pos, '-')) => SpannedToken::single_char(Token::Minus, pos),

            Some((pos, '=')) => self.emit_if_next('=', Token::EqualEqual, Token::Equal, pos),
            Some((pos, '!')) => self.emit_if_next('=', Token::BangEqual, Token::Equal, pos),
            Some((pos, '<')) => self.emit_if_next('=', Token::LessOrEqual, Token::LessThan, pos),
            Some((pos, '>')) => {
                self.emit_if_next('=', Token::GreaterOrEqual, Token::GreaterThan, pos)
            }

            Some((pos, c)) if is_identifier_start(c) => self.emit_identifier(pos, c),

            Some((pos, c)) => return Err(LexerError::UnexpectedChar { c, pos }),
            None => SpannedToken::eof(),
        };

        Ok(st)
    }
}

fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

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

#[derive(Debug)]
pub enum LexerError {
    UnexpectedChar { c: char, pos: usize },
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexerError::UnexpectedChar { c, pos } => {
                write!(f, "[pos: {}] Unexpected character `{}`", pos, c)
            }
        }
    }
}

impl std::error::Error for LexerError {}
