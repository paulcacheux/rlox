use crate::{ast, lexer::Span, tree_common as tc, CompilationContext};
use std::fmt;
use string_interner::DefaultSymbol;

#[derive(Debug)]
pub struct Evaluator<'c> {
    context: &'c CompilationContext,
}

impl<'c> Evaluator<'c> {
    pub fn new(context: &'c CompilationContext) -> Self {
        Evaluator { context }
    }

    pub fn eval_expression(&mut self, expr: &ast::Expression) -> Result<Value, EvalError> {
        match expr {
            ast::Expression::LazyLogical(inner) => self.eval_lazyop_expression(inner),
            ast::Expression::Binary(inner) => self.eval_binop_expression(inner),
            ast::Expression::Unary(inner) => self.eval_unaryop_expression(inner),
            ast::Expression::Literal(literal) => match literal.literal {
                tc::Literal::Number(value) => Ok(Value::Number(value)),
                tc::Literal::String(sym) => Ok(Value::String(sym)),
                tc::Literal::Bool(value) => Ok(Value::Bool(value)),
                tc::Literal::Nil => Ok(Value::Nil),
            },
            ast::Expression::Identifier(_) => todo!(),
        }
    }

    fn eval_lazyop_expression(
        &mut self,
        expr: &ast::LazyLogicalExpression,
    ) -> Result<Value, EvalError> {
        let lhs_value = self.eval_expression(&expr.lhs)?.to_bool();
        let res = match expr.operator {
            ast::LazyLogicalOperator::LogicalAnd => {
                if lhs_value {
                    self.eval_expression(&expr.rhs)?.to_bool()
                } else {
                    false
                }
            }
            ast::LazyLogicalOperator::LogicalOr => {
                if lhs_value {
                    true
                } else {
                    self.eval_expression(&expr.rhs)?.to_bool()
                }
            }
        };
        Ok(Value::Bool(res))
    }

    fn eval_binop_expression(&mut self, expr: &ast::BinaryExpression) -> Result<Value, EvalError> {
        let lhs_value = self.eval_expression(&expr.lhs)?;
        let rhs_value = self.eval_expression(&expr.rhs)?;

        match (expr.operator, lhs_value, rhs_value) {
            (ast::BinaryOperator::Add, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Number(a + b))
            }
            (ast::BinaryOperator::Add, Value::String(a), Value::String(b)) => {
                let res = self.context.resolve_str_symbol(a) + &self.context.resolve_str_symbol(b);
                let res_sym = self.context.intern_string(res);
                Ok(Value::String(res_sym))
            }
            (ast::BinaryOperator::Add, _, _) => Err(EvalError {
                msg: "Operands must be two numbers or two strings",
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::Substract, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Number(a - b))
            }
            (ast::BinaryOperator::Substract, _, _) => Err(EvalError {
                msg: "Operands must be two numbers",
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::Multiply, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Number(a * b))
            }
            (ast::BinaryOperator::Multiply, _, _) => Err(EvalError {
                msg: "Operands must be two numbers",
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::Divide, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Number(a / b))
            }
            (ast::BinaryOperator::Divide, _, _) => Err(EvalError {
                msg: "Operands must be two numbers",
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::LessThan, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Bool(a < b))
            }
            (ast::BinaryOperator::LessThan, _, _) => Err(EvalError {
                msg: "Operands must be two numbers",
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::LessOrEqual, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Bool(a <= b))
            }
            (ast::BinaryOperator::LessOrEqual, _, _) => Err(EvalError {
                msg: "Operands must be two numbers",
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::GreaterThan, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Bool(a > b))
            }
            (ast::BinaryOperator::GreaterThan, _, _) => Err(EvalError {
                msg: "Operands must be two numbers",
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::GreaterOrEqual, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Bool(a >= b))
            }
            (ast::BinaryOperator::GreaterOrEqual, _, _) => Err(EvalError {
                msg: "Operands must be two numbers",
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::Equal, a, b) => Ok(Value::Bool(a == b)),
            (ast::BinaryOperator::NotEqual, a, b) => Ok(Value::Bool(a != b)),
        }
    }

    fn eval_unaryop_expression(&mut self, expr: &ast::UnaryExpression) -> Result<Value, EvalError> {
        let sub = self.eval_expression(&expr.sub)?;

        match (expr.operator, sub) {
            (tc::UnaryOperator::Minus, Value::Number(a)) => Ok(Value::Number(-a)),
            (tc::UnaryOperator::Minus, _) => Err(EvalError {
                msg: "Operand must be a number",
                span: expr.operator_span,
            }),
            (tc::UnaryOperator::LogicalNot, Value::Bool(b)) => Ok(Value::Bool(!b)),
            (tc::UnaryOperator::LogicalNot, _) => Err(EvalError {
                msg: "Operand must be a boolean",
                span: expr.operator_span,
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Bool(bool),
    String(DefaultSymbol),
}

impl Value {
    fn to_bool(self) -> bool {
        match self {
            Value::Nil | Value::Bool(false) => false,
            _ => true,
        }
    }
}

#[derive(Debug)]
pub struct EvalError {
    pub msg: &'static str,
    pub span: Span,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[span: {:?}] {}", self.span, self.msg)
    }
}

impl std::error::Error for EvalError {}
