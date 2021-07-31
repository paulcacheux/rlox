mod comment_remover;
mod span;
mod token;

use core::fmt;
use std::{iter::Peekable, str::CharIndices};

pub use comment_remover::CommentRemover;
pub use span::{Span, SpannedToken};
pub use token::Token;

pub struct Lexer<I>
where
    I: Iterator<Item = (usize, char)>,
{
    src: Peekable<I>,
    buffer: Option<SpannedToken>,
}

impl<'s> Lexer<CommentRemover<CharIndices<'s>>> {
    pub fn new(content: &'s str) -> Self {
        Lexer {
            src: CommentRemover::new(content.char_indices()).peekable(),
            buffer: None,
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

    fn emit_number(&mut self, pos: usize, first_c: char) -> Result<SpannedToken, LexerError> {
        let mut value = String::new();
        value.push(first_c);
        let mut len = 1;
        let mut is_pointed = false;
        let mut point_at_end_position = None;

        while let Some(&(current_pos, c)) = self.src.peek() {
            if c.is_ascii_digit() || (c == '.' && !is_pointed) {
                self.src.next();
                value.push(c);
                len += 1;
                if c == '.' {
                    is_pointed = true;
                    point_at_end_position = Some(current_pos)
                } else {
                    point_at_end_position = None
                }
            } else {
                break;
            }
        }

        // Terrible hack to handle "123." that should emit "123" then "." without 2-char lookahead
        if let Some(point_at_end_pos) = point_at_end_position {
            self.buffer = Some(SpannedToken::single_char(Token::Dot, point_at_end_pos));
            value.pop();
            len -= 1;
        }

        let value = value
            .parse()
            .map_err(|_| LexerError::NumberParseError { pos })?;

        Ok(SpannedToken {
            token: Token::NumberLiteral(value),
            span: Span { pos, len },
        })
    }

    fn emit_string_literal(&mut self, pos: usize) -> SpannedToken {
        let mut value = String::new();
        let mut last_pos = pos;

        while let Some((current_pos, c)) = self.src.next() {
            last_pos = current_pos;
            if c != '"' {
                value.push(c);
            } else {
                break;
            }
        }

        SpannedToken {
            token: Token::StringLiteral(value),
            span: Span {
                pos,
                len: last_pos - pos + 1,
            },
        }
    }

    pub fn next_token(&mut self) -> Result<SpannedToken, LexerError> {
        if let Some(st) = self.buffer.take() {
            return Ok(st);
        }

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
            Some((pos, c)) if c.is_ascii_digit() => self.emit_number(pos, c)?,
            Some((pos, '"')) => self.emit_string_literal(pos),

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

#[derive(Debug)]
pub enum LexerError {
    UnexpectedChar { c: char, pos: usize },
    NumberParseError { pos: usize },
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexerError::UnexpectedChar { c, pos } => {
                write!(f, "[pos: {}] Unexpected character `{}`", pos, c)
            }
            LexerError::NumberParseError { pos } => {
                write!(f, "[pos: {}] Number parse error", pos)
            }
        }
    }
}

impl std::error::Error for LexerError {}
