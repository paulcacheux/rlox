use std::sync::Arc;

use crate::{ast, tree_common as tc, CompilationContext};

mod env;
mod error;
mod value;
pub use env::Environment;
pub use error::EvalError;
pub use value::Value;

#[derive(Debug)]
pub struct Evaluator<'c> {
    context: &'c CompilationContext,
    current_env: Arc<Environment>,
}

impl<'c> Evaluator<'c> {
    pub fn new(context: &'c CompilationContext) -> Self {
        let current_env = Environment::new();

        Evaluator {
            context,
            current_env,
        }
    }

    fn begin_env(&mut self) {
        self.current_env = Environment::with_parent(self.current_env.clone());
    }

    fn end_env(&mut self) {
        self.current_env = self
            .current_env
            .clone()
            .into_parent()
            .expect("Failed to switch to parent env");
    }

    pub fn value_to_str(&self, value: Value) -> String {
        match value {
            Value::Nil => "nil".into(),
            Value::Number(value) => value.to_string(),
            Value::Bool(value) => value.to_string(),
            Value::String(sym) => self.context.resolve_str_symbol(sym),
        }
    }

    pub fn eval_program(&mut self, prog: &ast::Program) -> Result<(), EvalError> {
        // top level env is already setup
        for stmt in &prog.statements {
            self.eval_statement(stmt)?;
        }
        Ok(())
    }

    pub fn eval_statement(&mut self, stmt: &ast::Statement) -> Result<(), EvalError> {
        match stmt {
            ast::Statement::VarDeclaration {
                identifier,
                init_expression,
                ..
            } => {
                let init_value = self.eval_expression(init_expression)?;
                self.current_env
                    .define_variable(identifier.identifier, init_value);
                Ok(())
            }
            ast::Statement::Block { statements, .. } => {
                self.begin_env();
                for stmt in statements {
                    self.eval_statement(stmt)?;
                }
                self.end_env();
                Ok(())
            }
            ast::Statement::If {
                condition,
                true_body,
                false_body,
                ..
            } => {
                let condition_value = self.eval_expression(condition)?;
                if condition_value.to_bool() {
                    self.eval_statement(true_body)?;
                } else {
                    self.eval_statement(false_body)?;
                }
                Ok(())
            }
            ast::Statement::Print { expression, .. } => {
                let print_value = self.eval_expression(expression)?;
                println!("{}", self.value_to_str(print_value));
                Ok(())
            }
            ast::Statement::Expression { expression, .. } => {
                if let Some(expr) = expression {
                    self.eval_expression(expr)?;
                }
                Ok(())
            }
        }
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
            ast::Expression::Identifier(ident) => {
                if let Some(value) = self.current_env.get_value(&ident.identifier) {
                    Ok(value)
                } else {
                    Err(EvalError {
                        msg: format!(
                            "Undefined identifier `{}`",
                            self.context.resolve_str_symbol(ident.identifier)
                        )
                        .into(),
                        span: ident.span,
                    })
                }
            }
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
                msg: "Operands must be two numbers or two strings".into(),
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::Substract, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Number(a - b))
            }
            (ast::BinaryOperator::Substract, _, _) => Err(EvalError {
                msg: "Operands must be two numbers".into(),
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::Multiply, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Number(a * b))
            }
            (ast::BinaryOperator::Multiply, _, _) => Err(EvalError {
                msg: "Operands must be two numbers".into(),
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::Divide, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Number(a / b))
            }
            (ast::BinaryOperator::Divide, _, _) => Err(EvalError {
                msg: "Operands must be two numbers".into(),
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::LessThan, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Bool(a < b))
            }
            (ast::BinaryOperator::LessThan, _, _) => Err(EvalError {
                msg: "Operands must be two numbers".into(),
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::LessOrEqual, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Bool(a <= b))
            }
            (ast::BinaryOperator::LessOrEqual, _, _) => Err(EvalError {
                msg: "Operands must be two numbers".into(),
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::GreaterThan, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Bool(a > b))
            }
            (ast::BinaryOperator::GreaterThan, _, _) => Err(EvalError {
                msg: "Operands must be two numbers".into(),
                span: expr.operator_span,
            }),

            (ast::BinaryOperator::GreaterOrEqual, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Bool(a >= b))
            }
            (ast::BinaryOperator::GreaterOrEqual, _, _) => Err(EvalError {
                msg: "Operands must be two numbers".into(),
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
                msg: "Operand must be a number".into(),
                span: expr.operator_span,
            }),
            (tc::UnaryOperator::LogicalNot, Value::Bool(b)) => Ok(Value::Bool(!b)),
            (tc::UnaryOperator::LogicalNot, _) => Err(EvalError {
                msg: "Operand must be a boolean".into(),
                span: expr.operator_span,
            }),
        }
    }
}
