use crate::ast;
use crate::lexer::Span;
use crate::parse_tree as pt;
use crate::tree_common as tc;
use crate::CompilationContext;

mod error;
mod scope;
use scope::Scopes;

use self::error::SemanticError;
use std::collections::HashSet;

#[derive(Debug)]
pub struct Translator<'c> {
    context: &'c CompilationContext,
    scopes: Scopes,
}

impl<'c> Translator<'c> {
    pub fn new(context: &'c CompilationContext) -> Self {
        Translator {
            context,
            scopes: Scopes::default(),
        }
    }

    pub fn translate_program(&mut self, prog: pt::Program) -> Result<ast::Program, SemanticError> {
        self.scopes.begin_scope();
        let statements: Result<Vec<_>, _> = prog
            .declarations
            .into_iter()
            .map(|decl| self.translate_declaration(decl))
            .collect();
        let statements = statements?;
        self.scopes.end_scope();

        Ok(ast::Program { statements })
    }

    pub fn translate_declaration(
        &mut self,
        declaration: pt::Declaration,
    ) -> Result<ast::Statement, SemanticError> {
        match declaration {
            pt::Declaration::Var(decl) => self.translate_var_declaration(decl),
            pt::Declaration::Statement(stmt) => self.translate_statement(stmt),
            pt::Declaration::Fun(fun) => self.translate_function_declaration(fun),
        }
    }

    pub fn translate_var_declaration(
        &mut self,
        var_decl: pt::VarDeclaration,
    ) -> Result<ast::Statement, SemanticError> {
        if !self
            .scopes
            .define_identifier(var_decl.identifier.identifier)
        {
            return Err(SemanticError::IdentifierAlreadyDefined {
                identifier: self
                    .context
                    .resolve_str_symbol(var_decl.identifier.identifier),
                identifier_span: var_decl.identifier.span,
            });
        }

        let (init_expression, equal_span) = if let Some(init) = var_decl.init {
            (
                self.translate_expression(*init.expression)?,
                init.equal_span,
            )
        } else {
            let expr = ast::Expression::Literal(tc::LiteralExpression {
                literal: tc::Literal::Nil,
                span: Span::INVALID,
            });
            (expr, Span::INVALID)
        };

        Ok(ast::Statement::VarDeclaration {
            var_keyword_span: var_decl.var_keyword_span,
            identifier: var_decl.identifier,
            init_expression: Box::new(init_expression),
            equal_span,
            semicolon_span: var_decl.semicolon_span,
        })
    }

    pub fn translate_function_declaration(
        &mut self,
        decl: pt::FunctionDeclaration,
    ) -> Result<ast::Statement, SemanticError> {
        let mut param_set = HashSet::new();
        for param in &decl.parameters {
            if param_set.contains(&param.identifier) {
                return Err(SemanticError::IdentifierAlreadyDefined {
                    identifier: self.context.resolve_str_symbol(param.identifier),
                    identifier_span: param.span,
                });
            }

            param_set.insert(param.identifier);
        }

        let body = self.translate_block_statement(*decl.body)?;

        Ok(ast::Statement::FunctionDeclaration {
            fun_keyword_span: decl.fun_keyword_span,
            function_name: decl.function_name,
            parameters: decl.parameters,
            left_parenthesis_span: decl.left_parenthesis_span,
            right_parenthesis_span: decl.right_parenthesis_span,
            body: Box::new(body),
        })
    }

    pub fn translate_statement(
        &mut self,
        statement: pt::Statement,
    ) -> Result<ast::Statement, SemanticError> {
        match statement {
            pt::Statement::Block(bs) => self.translate_block_statement(bs),
            pt::Statement::Expression(es) => self.translate_expression_statement(es),
            pt::Statement::If(is) => self.translate_if_statement(is),
            pt::Statement::While(ws) => self.translate_while_statement(ws),
            pt::Statement::For(fs) => self.translate_for_statement(fs),
            pt::Statement::Print(ps) => self.translate_print_statement(ps),
        }
    }

    pub fn translate_block_statement(
        &mut self,
        statement: pt::BlockStatement,
    ) -> Result<ast::Statement, SemanticError> {
        self.scopes.begin_scope();

        let statements: Result<Vec<_>, _> = statement
            .declarations
            .into_iter()
            .map(|decl| self.translate_declaration(decl))
            .collect();
        let statements = statements?;

        let bs = ast::Statement::Block {
            statements,
            left_bracket_span: statement.left_bracket_span,
            right_bracket_span: statement.right_bracket_span,
        };

        self.scopes.end_scope();

        Ok(bs)
    }

    pub fn translate_print_statement(
        &mut self,
        statement: pt::PrintStatement,
    ) -> Result<ast::Statement, SemanticError> {
        let expr = self.translate_expression(*statement.expression)?;
        Ok(ast::Statement::Print {
            expression: Box::new(expr),
            print_keyword_span: statement.print_keyword_span,
            semicolon_span: statement.semicolon_span,
        })
    }

    pub fn translate_expression_statement(
        &mut self,
        statement: pt::ExpressionStatement,
    ) -> Result<ast::Statement, SemanticError> {
        let expression = statement
            .expression
            .map(|expr| self.translate_expression(*expr).map(Box::new))
            .transpose()?;
        Ok(ast::Statement::Expression {
            expression,
            semicolon_span: statement.semicolon_span,
        })
    }

    pub fn translate_if_statement(
        &mut self,
        statement: pt::IfStatement,
    ) -> Result<ast::Statement, SemanticError> {
        let condition = self.translate_expression(*statement.condition)?;
        let true_body = self.translate_statement(*statement.body)?;

        let (false_body, else_keyword_span) = if let Some(else_part) = statement.else_statement {
            let body = self.translate_statement(*else_part.body)?;
            (body, else_part.else_keyword_span)
        } else {
            let empty_body = ast::Statement::Block {
                statements: Vec::new(),
                left_bracket_span: Span::INVALID,
                right_bracket_span: Span::INVALID,
            };
            (empty_body, Span::INVALID)
        };

        Ok(ast::Statement::If {
            condition: Box::new(condition),
            if_keyword_span: statement.if_keyword_span,
            left_paren_span: statement.left_paren_span,
            right_paren_span: statement.right_paren_span,
            true_body: Box::new(true_body),
            else_keyword_span,
            false_body: Box::new(false_body),
        })
    }

    pub fn translate_while_statement(
        &mut self,
        statement: pt::WhileStatement,
    ) -> Result<ast::Statement, SemanticError> {
        let condition = self.translate_expression(*statement.condition)?;
        let body = self.translate_statement(*statement.body)?;

        Ok(ast::Statement::While {
            condition: Box::new(condition),
            while_keyword_span: statement.while_keyword_span,
            left_paren_span: statement.left_paren_span,
            right_paren_span: statement.right_paren_span,
            body: Box::new(body),
        })
    }

    pub fn translate_for_statement(
        &mut self,
        statement: pt::ForStatement,
    ) -> Result<ast::Statement, SemanticError> {
        let init = match *statement.init {
            pt::VarDeclOrExpressionStatement::Var(var_decl) => {
                self.translate_var_declaration(var_decl)?
            }
            pt::VarDeclOrExpressionStatement::Expr(expr_stmt) => {
                self.translate_expression_statement(expr_stmt)?
            }
        };

        let condition = if let Some(expr) = statement.condition.expression {
            self.translate_expression(*expr)?
        } else {
            ast::Expression::Literal(tc::LiteralExpression {
                literal: tc::Literal::Bool(true),
                span: statement.condition.semicolon_span,
            })
        };

        let body = self.translate_statement(*statement.body)?;

        let res_loop_body = if let Some(step) = statement.step {
            let step = self.translate_expression(*step)?;

            ast::Statement::Block {
                statements: vec![
                    body,
                    ast::Statement::Expression {
                        expression: Some(step.into()),
                        semicolon_span: Span::INVALID,
                    },
                ],
                left_bracket_span: Span::INVALID,
                right_bracket_span: Span::INVALID,
            }
        } else {
            body
        };

        let while_loop = ast::Statement::While {
            condition: condition.into(),
            while_keyword_span: statement.for_keyword_span,
            left_paren_span: statement.left_paren_span,
            right_paren_span: statement.right_paren_span,
            body: res_loop_body.into(),
        };

        let res = ast::Statement::Block {
            statements: vec![init, while_loop],
            left_bracket_span: Span::INVALID,
            right_bracket_span: Span::INVALID,
        };

        Ok(res)
    }

    pub fn translate_expression(
        &mut self,
        expr: pt::Expression,
    ) -> Result<ast::Expression, SemanticError> {
        match expr {
            pt::Expression::Assignment(assign_expr) => {
                self.translate_assignment_expression(assign_expr)
            }
            pt::Expression::Parenthesis(pt::ParenthesisExpression { sub, .. }) => {
                self.translate_expression(*sub)
            }
            pt::Expression::Binary(bin_expr) => self.translate_binary_expression(bin_expr),
            pt::Expression::Unary(pt::UnaryExpression {
                operator,
                operator_span,
                sub,
            }) => Ok(ast::Expression::Unary(ast::UnaryExpression {
                operator,
                operator_span,
                sub: Box::new(self.translate_expression(*sub)?),
            })),
            pt::Expression::Literal(literal) => Ok(ast::Expression::Literal(literal)),
            pt::Expression::Identifier(id) => Ok(ast::Expression::Identifier(id)),
            pt::Expression::Call(call) => Ok(ast::Expression::Call(ast::CallExpression {
                function: Box::new(self.translate_expression(*call.function)?),
                arguments: call
                    .arguments
                    .into_iter()
                    .map(|arg| self.translate_expression(arg))
                    .collect::<Result<Vec<_>, _>>()?,
                left_parenthesis_span: call.left_parenthesis_span,
                right_parenthesis_span: call.right_parenthesis_span,
            })),
        }
    }

    fn translate_assignment_expression(
        &mut self,
        expr: pt::AssignmentExpression,
    ) -> Result<ast::Expression, SemanticError> {
        let lhs = self.translate_expression(*expr.lhs)?;
        let rhs = self.translate_expression(*expr.rhs)?;

        if let Some(assign) = lhs.into_assign() {
            Ok(ast::Expression::AssignExpression(ast::AssignExpression {
                equal_span: expr.equal_span,
                lhs: Box::new(assign),
                rhs: Box::new(rhs),
            }))
        } else {
            Err(SemanticError::LhsNotAssignable {
                lhs_span: expr.equal_span,
            })
        }
    }

    fn translate_binary_expression(
        &mut self,
        expr: pt::BinaryExpression,
    ) -> Result<ast::Expression, SemanticError> {
        let pt::BinaryExpression {
            operator,
            operator_span,
            lhs,
            rhs,
        } = expr;

        enum LogicalOrBinary {
            Logical(ast::LazyLogicalOperator),
            Binary(ast::BinaryOperator),
        }

        let lhs = Box::new(self.translate_expression(*lhs)?);
        let rhs = Box::new(self.translate_expression(*rhs)?);
        let op = match operator {
            pt::BinaryOperator::LogicalAnd => {
                LogicalOrBinary::Logical(ast::LazyLogicalOperator::LogicalAnd)
            }
            pt::BinaryOperator::LogicalOr => {
                LogicalOrBinary::Logical(ast::LazyLogicalOperator::LogicalOr)
            }
            pt::BinaryOperator::NotEqual => LogicalOrBinary::Binary(ast::BinaryOperator::NotEqual),
            pt::BinaryOperator::Equal => LogicalOrBinary::Binary(ast::BinaryOperator::Equal),
            pt::BinaryOperator::LessThan => LogicalOrBinary::Binary(ast::BinaryOperator::LessThan),
            pt::BinaryOperator::LessOrEqual => {
                LogicalOrBinary::Binary(ast::BinaryOperator::LessOrEqual)
            }
            pt::BinaryOperator::GreaterThan => {
                LogicalOrBinary::Binary(ast::BinaryOperator::GreaterThan)
            }
            pt::BinaryOperator::GreaterOrEqual => {
                LogicalOrBinary::Binary(ast::BinaryOperator::GreaterOrEqual)
            }
            pt::BinaryOperator::Add => LogicalOrBinary::Binary(ast::BinaryOperator::Add),
            pt::BinaryOperator::Substract => {
                LogicalOrBinary::Binary(ast::BinaryOperator::Substract)
            }
            pt::BinaryOperator::Multiply => LogicalOrBinary::Binary(ast::BinaryOperator::Multiply),
            pt::BinaryOperator::Divide => LogicalOrBinary::Binary(ast::BinaryOperator::Divide),
        };

        Ok(match op {
            LogicalOrBinary::Binary(op) => ast::Expression::Binary(ast::BinaryExpression {
                operator: op,
                operator_span,
                lhs,
                rhs,
            }),
            LogicalOrBinary::Logical(op) => {
                ast::Expression::LazyLogical(ast::LazyLogicalExpression {
                    operator: op,
                    operator_span,
                    lhs,
                    rhs,
                })
            }
        })
    }
}
