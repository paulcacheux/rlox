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

    #[inline(always)]
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

    #[inline(always)]
    fn front_matches(&mut self, token: Token) -> Result<bool, ParseError> {
        let front_st = self.lexer.peek_token()?;
        Ok(front_st.token == token)
    }

    pub fn parse_program(&mut self) -> Result<pt::Program, ParseError> {
        let mut declarations = Vec::new();
        loop {
            if self.front_matches(Token::EndOfFile)? {
                break;
            }

            let decl = self.parse_declaration()?;
            declarations.push(decl);
        }

        Ok(pt::Program { declarations })
    }

    pub fn parse_declaration(&mut self) -> Result<pt::Declaration, ParseError> {
        let front_st = self.lexer.peek_token()?;
        let decl = match front_st.token {
            Token::VarKeyword => pt::Declaration::Var(self.parse_var_declaration()?),
            _ => pt::Declaration::Statement(self.parse_statement()?),
        };
        Ok(decl)
    }

    fn parse_var_declaration(&mut self) -> Result<pt::VarDeclaration, ParseError> {
        let var_keyword_span = self.expect(Token::VarKeyword)?;

        let front_st = self.lexer.next_token()?;
        let identifier = if let Token::Identifier(identifier) = front_st.token {
            tc::IdentifierExpression {
                identifier,
                span: front_st.span,
            }
        } else {
            return Err(ParseError::UnexpectedSyntax {
                msg: "Expected identifier",
                got: front_st,
            });
        };

        let front_st = self.lexer.peek_token()?;
        let init = if front_st.token == Token::Equal {
            let equal_span = self.expect(Token::Equal)?;
            let expression = self.parse_expression()?;
            Some(pt::VarInit {
                expression: Box::new(expression),
                equal_span,
            })
        } else {
            None
        };

        let semicolon_span = self.expect(Token::SemiColon)?;
        Ok(pt::VarDeclaration {
            var_keyword_span,
            identifier,
            init,
            semicolon_span,
        })
    }

    pub fn parse_statement(&mut self) -> Result<pt::Statement, ParseError> {
        let front_st = self.lexer.peek_token()?;

        let stmt = match front_st.token {
            Token::IfKeyword => pt::Statement::If(self.parse_if_statement()?),
            Token::WhileKeyword => pt::Statement::While(self.parse_while_statement()?),
            Token::ForKeyword => pt::Statement::For(self.parse_for_statement()?),
            Token::PrintKeyword => pt::Statement::Print(self.parse_print_statement()?),
            Token::LeftBracket => pt::Statement::Block(self.parse_block_statement()?),
            _ => pt::Statement::Expression(self.parse_expression_statement()?),
        };
        Ok(stmt)
    }

    pub fn parse_if_statement(&mut self) -> Result<pt::IfStatement, ParseError> {
        let if_keyword_span = self.expect(Token::IfKeyword)?;
        let left_paren_span = self.expect(Token::LeftParenthesis)?;
        let condition = self.parse_expression()?;
        let right_paren_span = self.expect(Token::RightParenthesis)?;
        let body = self.parse_statement()?;

        let front_st = self.lexer.peek_token()?;
        let else_statement = if front_st.token == Token::ElseKeyword {
            let else_keyword_span = self.expect(Token::ElseKeyword)?;
            let body = self.parse_statement()?;
            Some(pt::ElseStatement {
                else_keyword_span,
                body: Box::new(body),
            })
        } else {
            None
        };

        Ok(pt::IfStatement {
            condition: Box::new(condition),
            if_keyword_span,
            left_paren_span,
            right_paren_span,
            body: Box::new(body),
            else_statement,
        })
    }

    pub fn parse_while_statement(&mut self) -> Result<pt::WhileStatement, ParseError> {
        let while_keyword_span = self.expect(Token::WhileKeyword)?;
        let left_paren_span = self.expect(Token::LeftParenthesis)?;
        let condition = self.parse_expression()?;
        let right_paren_span = self.expect(Token::RightParenthesis)?;
        let body = self.parse_statement()?;

        Ok(pt::WhileStatement {
            condition: Box::new(condition),
            while_keyword_span,
            left_paren_span,
            right_paren_span,
            body: Box::new(body),
        })
    }

    pub fn parse_for_statement(&mut self) -> Result<pt::ForStatement, ParseError> {
        let for_keyword_span = self.expect(Token::ForKeyword)?;
        let left_paren_span = self.expect(Token::LeftParenthesis)?;

        let front_st = self.lexer.peek_token()?;
        let init = match front_st.token {
            Token::VarKeyword => {
                pt::VarDeclOrExpressionStatement::Var(self.parse_var_declaration()?)
            }
            _ => {
                pt::VarDeclOrExpressionStatement::Expr(self.parse_expression_or_empty_statement()?)
            }
        };
        let condition = self.parse_expression_or_empty_statement()?;

        let step = if !self.front_matches(Token::RightParenthesis)? {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let right_paren_span = self.expect(Token::RightParenthesis)?;
        let body = self.parse_statement()?;

        Ok(pt::ForStatement {
            init: Box::new(init),
            condition: Box::new(condition),
            step: step.map(Box::new),
            for_keyword_span,
            left_paren_span,
            right_paren_span,
            body: Box::new(body),
        })
    }

    pub fn parse_block_statement(&mut self) -> Result<pt::BlockStatement, ParseError> {
        let left_bracket_span = self.expect(Token::LeftBracket)?;

        let mut declarations = Vec::new();
        loop {
            if self.front_matches(Token::RightBracket)? {
                break;
            }

            let stmt = self.parse_declaration()?;
            declarations.push(stmt);
        }

        let right_bracket_span = self.expect(Token::RightBracket)?;
        Ok(pt::BlockStatement {
            declarations,
            left_bracket_span,
            right_bracket_span,
        })
    }

    pub fn parse_print_statement(&mut self) -> Result<pt::PrintStatement, ParseError> {
        let print_keyword_span = self.expect(Token::PrintKeyword)?;
        let expression = self.parse_expression()?;
        let semicolon_span = self.expect(Token::SemiColon)?;
        Ok(pt::PrintStatement {
            expression: Box::new(expression),
            print_keyword_span,
            semicolon_span,
        })
    }

    pub fn parse_expression_or_empty_statement(
        &mut self,
    ) -> Result<pt::ExpressionStatement, ParseError> {
        if self.front_matches(Token::SemiColon)? {
            let semicolon_span = self.expect(Token::SemiColon)?;
            Ok(pt::ExpressionStatement {
                expression: None,
                semicolon_span,
            })
        } else {
            self.parse_expression_statement()
        }
    }

    pub fn parse_expression_statement(&mut self) -> Result<pt::ExpressionStatement, ParseError> {
        let expression = self.parse_expression()?;
        let semicolon_span = self.expect(Token::SemiColon)?;
        Ok(pt::ExpressionStatement {
            expression: Some(Box::new(expression)),
            semicolon_span,
        })
    }

    pub fn parse_expression(&mut self) -> Result<pt::Expression, ParseError> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> Result<pt::Expression, ParseError> {
        let mut lhs = self.parse_logic_or()?;
        if self.front_matches(Token::Equal)? {
            let equal_span = self.expect(Token::Equal)?;

            let rhs = self.parse_assignment_expression()?;

            lhs = pt::Expression::Assignment(pt::AssignmentExpression {
                equal_span,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
        }
        Ok(lhs)
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
