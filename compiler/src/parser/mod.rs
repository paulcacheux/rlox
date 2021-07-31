use std::str::CharIndices;

use crate::{
    lexer::{CommentRemover, PeekableLexer, Span, SpannedToken, Token},
    parse_tree as pt,
};
pub use error::ParseError;

mod error;

type ParserLexer<'c, 's> = PeekableLexer<'c, CommentRemover<CharIndices<'s>>>;

#[derive(Debug)]
pub struct Parser<'c, 's> {
    lexer: ParserLexer<'c, 's>,
    errors: Vec<ParseError>,
}

impl<'c, 's> Parser<'c, 's> {
    pub fn new(lexer: ParserLexer<'c, 's>) -> Self {
        Parser {
            lexer,
            errors: Vec::new(),
        }
    }

    fn next_token(&mut self, sync: bool) -> Result<SpannedToken, ParserErrorCommand> {
        match self.lexer.next_token() {
            Ok(st) => Ok(st),
            Err(err) => {
                self.push_error(err.into());
                Err(ParserErrorCommand::from_sync(sync))
            }
        }
    }

    fn peek_token(&mut self, sync: bool) -> Result<SpannedToken, ParserErrorCommand> {
        match self.lexer.peek_token() {
            Ok(st) => Ok(st),
            Err(err) => {
                self.push_error(err.into());
                Err(ParserErrorCommand::from_sync(sync))
            }
        }
    }

    fn advance_lexer(&mut self) {
        let _ = self.lexer.next_token();
    }

    fn expect(&mut self, expected: Token, sync: bool) -> Result<Span, ParserErrorCommand> {
        let next_st = self.next_token(sync)?;
        if next_st.token == expected {
            Ok(next_st.span)
        } else {
            self.push_error(ParseError::UnexpectedToken {
                expected,
                got: next_st,
            });
            Err(ParserErrorCommand::from_sync(sync))
        }
    }

    pub fn parse_expression(&mut self) -> Result<pt::Expression, ParserErrorCommand> {
        unimplemented!()
    }

    fn parse_unary(&mut self) -> Result<pt::Expression, ParserErrorCommand> {
        let front_st = self.peek_token(true)?;
        match front_st.token {
            Token::Bang => {
                self.advance_lexer();
                let sub = self.parse_unary()?;
                Ok(pt::Expression::Unary(pt::UnaryExpression {
                    operator: pt::UnaryOperator::LogicalNot,
                    operator_span: front_st.span,
                    sub: Box::new(sub),
                }))
            }
            Token::Minus => {
                self.advance_lexer();
                let sub = self.parse_unary()?;
                Ok(pt::Expression::Unary(pt::UnaryExpression {
                    operator: pt::UnaryOperator::Minus,
                    operator_span: front_st.span,
                    sub: Box::new(sub),
                }))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<pt::Expression, ParserErrorCommand> {
        let front_st = self.next_token(true)?;
        match front_st.token {
            Token::NumberLiteral(value) => Ok(utils::build_literal(
                pt::Literal::Number(value),
                front_st.span,
            )),
            Token::BoolLiteral(value) => Ok(utils::build_literal(
                pt::Literal::Bool(value),
                front_st.span,
            )),
            Token::NilKeyword => Ok(utils::build_literal(pt::Literal::Nil, front_st.span)),
            Token::StringLiteral(sym) => Ok(utils::build_literal(
                pt::Literal::String(sym),
                front_st.span,
            )),
            Token::Identifier(sym) => Ok(pt::Expression::Identifier(pt::IdentifierExpression {
                identifier: sym,
                span: front_st.span,
            })),
            Token::LeftParenthesis => self.continue_parse_parenthesis_expr(front_st.span),

            other => {
                self.push_error(ParseError::UnexpectedSyntax {
                    msg: "Expected a literal, or `(`",
                    got: front_st,
                });
                Err(ParserErrorCommand::from_sync(other != Token::SemiColon))
            }
        }
    }

    fn continue_parse_parenthesis_expr(
        &mut self,
        left_span: Span,
    ) -> Result<pt::Expression, ParserErrorCommand> {
        let sub = self.parse_expression()?;
        let right_span = self.expect(Token::RightParenthesis, true)?;

        Ok(pt::Expression::Parenthesis(pt::ParenthesisExpression {
            left_paren_span: left_span,
            right_paren_span: right_span,
            sub: Box::new(sub),
        }))
    }

    pub fn push_error(&mut self, error: ParseError) {
        self.errors.push(error);
    }
}

#[derive(Debug)]
pub enum ParserErrorCommand {
    ErrorEndOfExpression,
    ErrorMidExpression,
}

impl ParserErrorCommand {
    fn from_sync(sync: bool) -> Self {
        if sync {
            ParserErrorCommand::ErrorMidExpression
        } else {
            ParserErrorCommand::ErrorEndOfExpression
        }
    }
}

mod utils {
    use crate::{lexer::Span, parse_tree as pt};

    pub fn build_literal(literal: pt::Literal, span: Span) -> pt::Expression {
        pt::Expression::Literal(pt::LiteralExpression { literal, span })
    }
}
