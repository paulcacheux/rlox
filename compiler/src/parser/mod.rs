use std::str::CharIndices;

use crate::{
    lexer::{CommentRemover, PeekableLexer, Span, Token},
    parse_tree as pt, tree_common as tc,
};
pub use error::ParseError;

mod error;

type ParserLexer<'c, 's> = PeekableLexer<'c, CommentRemover<CharIndices<'s>>>;

#[derive(Debug)]
pub struct Parser<'c, 's> {
    lexer: ParserLexer<'c, 's>,
}

macro_rules! parse_expr_fn {
    ($fn_name:ident, $sub_fn:ident, $($tok:pat => $op:expr,)+) => {
        fn $fn_name(&mut self) -> Result<pt::Expression, ParseError> {
            let mut lhs = self.$sub_fn()?;

            loop {
                let next_st = self.lexer.peek_token()?;
                let operator = match next_st.token {
                    $($tok => $op,)+
                    _ => break,
                };

                self.advance_lexer();
                let rhs = self.$sub_fn()?;
                lhs = pt::Expression::Binary(pt::BinaryExpression {
                    operator,
                    operator_span: next_st.span,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                });
            }

            Ok(lhs)
        }
    };
}

impl<'c, 's> Parser<'c, 's> {
    pub fn new(lexer: ParserLexer<'c, 's>) -> Self {
        Parser { lexer }
    }

    fn advance_lexer(&mut self) {
        let _ = self.lexer.next_token();
    }

    fn expect(&mut self, expected: Token) -> Result<Span, ParseError> {
        let next_st = self.lexer.next_token()?;
        if next_st.token == expected {
            Ok(next_st.span)
        } else {
            Err(ParseError::UnexpectedToken {
                expected,
                got: next_st,
            })
        }
    }

    pub fn parse_statement(&mut self) -> Result<pt::Statement, ParseError> {
        let front_st = self.lexer.peek_token()?;

        match front_st.token {
            // Token::IfKeyword => self.parse_if_statement(),
            Token::PrintKeyword => self.parse_print_statement(),
            Token::LeftBracket => self.parse_block_statement(),
            _ => todo!(),
        }
    }

    pub fn parse_block_statement(&mut self) -> Result<pt::Statement, ParseError> {
        let left_bracket_span = self.expect(Token::LeftBracket)?;

        let mut statements = Vec::new();
        loop {
            let front_st = self.lexer.peek_token()?;
            if front_st.token != Token::RightBracket {
                let stmt = self.parse_statement()?;
                statements.push(stmt);
            } else {
                break;
            }
        }

        let right_bracket_span = self.expect(Token::RightBracket)?;
        Ok(pt::Statement::Block(pt::BlockStatement {
            statements,
            left_bracket_span,
            right_bracket_span,
        }))
    }

    pub fn parse_print_statement(&mut self) -> Result<pt::Statement, ParseError> {
        let print_keyword_span = self.expect(Token::PrintKeyword)?;
        let expression = self.parse_expression()?;
        let semicolon_span = self.expect(Token::SemiColon)?;
        Ok(pt::Statement::Print(pt::PrintStatement {
            expression: Box::new(expression),
            print_keyword_span,
            semicolon_span,
        }))
    }

    pub fn parse_expression_statement(&mut self) -> Result<pt::Statement, ParseError> {
        let expression = self.parse_expression()?;
        let semicolon_span = self.expect(Token::SemiColon)?;
        Ok(pt::Statement::Expression(pt::ExpressionStatement {
            expression: Some(Box::new(expression)),
            semicolon_span,
        }))
    }

    pub fn parse_expression(&mut self) -> Result<pt::Expression, ParseError> {
        self.parse_logic_or()
    }

    parse_expr_fn!(parse_logic_or, parse_logic_and,
        Token::OrKeyword => pt::BinaryOperator::LogicalOr,
    );

    parse_expr_fn!(parse_logic_and, parse_equality,
        Token::AndKeyword => pt::BinaryOperator::LogicalAnd,
    );

    parse_expr_fn!(parse_equality, parse_comparison,
        Token::BangEqual => pt::BinaryOperator::NotEqual,
        Token::EqualEqual => pt::BinaryOperator::Equal,
    );

    parse_expr_fn!(parse_comparison, parse_term,
        Token::LessThan => pt::BinaryOperator::LessThan,
        Token::LessOrEqual => pt::BinaryOperator::LessOrEqual,
        Token::GreaterThan => pt::BinaryOperator::GreaterThan,
        Token::GreaterOrEqual => pt::BinaryOperator::GreaterOrEqual,
    );

    parse_expr_fn!(parse_term, parse_factor,
        Token::Minus => pt::BinaryOperator::Substract,
        Token::Plus => pt::BinaryOperator::Add,
    );

    fn parse_factor(&mut self) -> Result<pt::Expression, ParseError> {
        let mut lhs = self.parse_unary()?;

        loop {
            let next_st = self.lexer.peek_token()?;
            let operator = match next_st.token {
                Token::Slash => pt::BinaryOperator::Divide,
                Token::Star => pt::BinaryOperator::Multiply,
                _ => break,
            };

            self.advance_lexer();
            let rhs = self.parse_unary()?;
            lhs = pt::Expression::Binary(pt::BinaryExpression {
                operator,
                operator_span: next_st.span,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
        }

        Ok(lhs)
    }

    fn parse_unary(&mut self) -> Result<pt::Expression, ParseError> {
        let front_st = self.lexer.peek_token()?;
        match front_st.token {
            Token::Bang => {
                self.advance_lexer();
                let sub = self.parse_unary()?;
                Ok(pt::Expression::Unary(pt::UnaryExpression {
                    operator: tc::UnaryOperator::LogicalNot,
                    operator_span: front_st.span,
                    sub: Box::new(sub),
                }))
            }
            Token::Minus => {
                self.advance_lexer();
                let sub = self.parse_unary()?;
                Ok(pt::Expression::Unary(pt::UnaryExpression {
                    operator: tc::UnaryOperator::Minus,
                    operator_span: front_st.span,
                    sub: Box::new(sub),
                }))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<pt::Expression, ParseError> {
        let front_st = self.lexer.next_token()?;
        match front_st.token {
            Token::NumberLiteral(value) => Ok(utils::build_literal(
                tc::Literal::Number(value),
                front_st.span,
            )),
            Token::BoolLiteral(value) => Ok(utils::build_literal(
                tc::Literal::Bool(value),
                front_st.span,
            )),
            Token::NilKeyword => Ok(utils::build_literal(tc::Literal::Nil, front_st.span)),
            Token::StringLiteral(sym) => Ok(utils::build_literal(
                tc::Literal::String(sym),
                front_st.span,
            )),
            Token::Identifier(sym) => Ok(pt::Expression::Identifier(tc::IdentifierExpression {
                identifier: sym,
                span: front_st.span,
            })),
            Token::LeftParenthesis => self.continue_parse_parenthesis_expr(front_st.span),
            _ => Err(ParseError::UnexpectedSyntax {
                msg: "Expected a literal, or `(`",
                got: front_st,
            }),
        }
    }

    fn continue_parse_parenthesis_expr(
        &mut self,
        left_span: Span,
    ) -> Result<pt::Expression, ParseError> {
        let sub = self.parse_expression()?;
        let right_span = self.expect(Token::RightParenthesis)?;

        Ok(pt::Expression::Parenthesis(pt::ParenthesisExpression {
            left_paren_span: left_span,
            right_paren_span: right_span,
            sub: Box::new(sub),
        }))
    }
}

mod utils {
    use crate::{lexer::Span, parse_tree as pt, tree_common as tc};

    pub fn build_literal(literal: tc::Literal, span: Span) -> pt::Expression {
        pt::Expression::Literal(tc::LiteralExpression { literal, span })
    }
}
