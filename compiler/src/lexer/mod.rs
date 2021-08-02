mod comment_remover;
mod peekable;
mod span;
mod token;

use core::fmt;
use std::{iter::Peekable, str::CharIndices};

pub use comment_remover::CommentRemover;
pub use peekable::PeekableLexer;
pub use span::{Span, SpannedToken};
pub use token::Token;

use crate::CompilationContext;

#[derive(Debug, Clone)]
pub struct Lexer<'c, I>
where
    I: Iterator<Item = (usize, char)>,
{
    ctx: &'c CompilationContext,
    src: Peekable<I>,
    buffer: Option<SpannedToken>,
}

impl<'c, 's> Lexer<'c, CommentRemover<CharIndices<'s>>> {
    pub fn new(ctx: &'c CompilationContext, content: &'s str) -> Self {
        Lexer {
            ctx,
            src: CommentRemover::new(content.char_indices()).peekable(),
            buffer: None,
        }
    }
}

impl<'c, I> Lexer<'c, I>
where
    I: Iterator<Item = (usize, char)>,
{
    pub fn peekable(self) -> PeekableLexer<'c, I> {
        PeekableLexer::new(self)
    }

    fn next_skip_whitespaces(&mut self) -> Option<(usize, char)> {
        for (pos, c) in &mut self.src {
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
        begin: usize,
    ) -> SpannedToken {
        match self.src.peek() {
            Some(&(current_pos, next_c)) if next_c == condition => {
                self.src.next();
                SpannedToken {
                    token: if_true,
                    span: Span {
                        begin,
                        end: current_pos + next_c.len_utf8(),
                    },
                }
            }
            _ => SpannedToken::single_char(if_false, begin),
        }
    }

    fn emit_identifier(&mut self, first_c: char, begin: usize) -> SpannedToken {
        let mut identifier = String::new();
        identifier.push(first_c);
        let mut end = begin + first_c.len_utf8();

        while let Some(&(current_pos, c)) = self.src.peek() {
            if is_identifier_continue(c) {
                self.src.next();
                identifier.push(c);
                end = current_pos.saturating_add(c.len_utf8());
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
            _ => Token::Identifier(self.ctx.intern_string(identifier)),
        };

        SpannedToken {
            token,
            span: Span { begin, end },
        }
    }

    fn emit_number(&mut self, first_c: char, begin: usize) -> Result<SpannedToken, LexerError> {
        let mut value = String::new();
        value.push(first_c);
        let mut is_pointed = false;
        let mut point_at_end_position = None;
        let mut end = begin + first_c.len_utf8();

        while let Some(&(current_pos, c)) = self.src.peek() {
            if c.is_ascii_digit() || (c == '.' && !is_pointed) {
                self.src.next();
                value.push(c);
                end = current_pos + c.len_utf8();
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
            if let Some(popped) = value.pop() {
                end -= popped.len_utf8();
            }
        }

        let span = Span { begin, end };

        let value = value
            .parse()
            .map_err(|_| LexerError::NumberParseError { span })?;

        Ok(SpannedToken {
            token: Token::NumberLiteral(value),
            span,
        })
    }

    fn emit_string_literal(&mut self, begin: usize) -> SpannedToken {
        let mut value = String::new();
        let mut end = begin + '"'.len_utf8();

        for (current_pos, c) in &mut self.src {
            end = current_pos + c.len_utf8();
            if c != '"' {
                value.push(c);
            } else {
                break;
            }
        }

        let value_symbol = self.ctx.intern_string(value);
        SpannedToken {
            token: Token::StringLiteral(value_symbol),
            span: Span { begin, end },
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
            Some((pos, '!')) => self.emit_if_next('=', Token::BangEqual, Token::Bang, pos),
            Some((pos, '<')) => self.emit_if_next('=', Token::LessOrEqual, Token::LessThan, pos),
            Some((pos, '>')) => {
                self.emit_if_next('=', Token::GreaterOrEqual, Token::GreaterThan, pos)
            }

            Some((pos, c)) if is_identifier_start(c) => self.emit_identifier(c, pos),
            Some((pos, c)) if c.is_ascii_digit() => self.emit_number(c, pos)?,
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
    NumberParseError { span: Span },
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexerError::UnexpectedChar { c, pos } => {
                write!(f, "[pos: {}] Unexpected character `{}`", pos, c)
            }
            LexerError::NumberParseError { span } => {
                write!(f, "[span: {:?}] Number parse error", span)
            }
        }
    }
}

impl std::error::Error for LexerError {}
