use std::{io::Write, sync::Arc};

use crate::{ast, tree_common as tc, CompilationContext};

mod env;
mod error;
mod function;
mod value;
pub use env::Environment;
pub use error::EvalError;
use function::FunctionEvaluator;
pub use value::Value;

#[derive(Debug)]
pub struct Evaluator<'c, W: Write> {
    context: &'c CompilationContext,
    functions: Vec<FunctionEvaluator>,
    current_env: Arc<Environment>,
    stdout: W,
}

macro_rules! flow {
    ($stmt_eval:expr) => {
        match $stmt_eval {
            ret @ Ok(StatementControlFlow::Return(_)) => return ret,
            err @ Err(_) => return err,
            Ok(StatementControlFlow::Continue) => {}
        }
    };
}

impl<'c, W: Write> Evaluator<'c, W> {
    pub fn new(context: &'c CompilationContext, stdout: W) -> Self {
        let current_env = Environment::new();

        Evaluator {
            context,
            functions: Vec::new(),
            current_env,
            stdout,
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

    fn create_function_ref(&mut self, evaluator: FunctionEvaluator) -> Value {
        let index = self.functions.len();
        self.functions.push(evaluator);
        Value::FunctionRef(index)
    }

    fn eval_with_env<T, F>(&mut self, env: Arc<Environment>, eval: F) -> Result<T, EvalError>
    where
        F: Fn(&mut Self) -> Result<T, EvalError>,
    {
        let save_env = std::mem::replace(&mut self.current_env, env);
        let res = eval(self)?;
        self.current_env = save_env;
        Ok(res)
    }

    pub fn value_to_str(&self, value: Value) -> String {
        match value {
            Value::Nil => "nil".into(),
            Value::Number(value) => value.to_string(),
            Value::Bool(value) => value.to_string(),
            Value::String(sym) => self.context.resolve_str_symbol(sym),
            Value::FunctionRef(index) => format!("<fn {}>", index),
        }
    }

    pub fn eval_program(&mut self, prog: &ast::Program) -> Result<(), EvalError> {
        // top level env is already setup
        for stmt in &prog.statements {
            self.eval_statement(stmt)?;
        }
        Ok(())
    }

    pub fn eval_statement(
        &mut self,
        stmt: &ast::Statement,
    ) -> Result<StatementControlFlow, EvalError> {
        match stmt {
            ast::Statement::VarDeclaration {
                identifier,
                init_expression,
                ..
            } => {
                let init_value = self.eval_expression(init_expression)?;
                self.current_env
                    .define_variable(identifier.identifier, init_value);
                Ok(StatementControlFlow::Continue)
            }
            ast::Statement::FunctionDeclaration {
                function_name,
                parameters,
                body,
                ..
            } => {
                let fun_eval = FunctionEvaluator {
                    parameters: parameters.iter().map(|ident| ident.identifier).collect(),
                    body: body.clone(),
                };
                let fun_ref = self.create_function_ref(fun_eval);
                self.current_env
                    .define_variable(function_name.identifier, fun_ref);
                Ok(StatementControlFlow::Continue)
            }
            ast::Statement::Block { statements, .. } => {
                self.begin_env();
                for stmt in statements {
                    flow!(self.eval_statement(stmt));
                }
                self.end_env();
                Ok(StatementControlFlow::Continue)
            }
            ast::Statement::If {
                condition,
                true_body,
                false_body,
                ..
            } => {
                if self.eval_expression(condition)?.to_bool() {
                    flow!(self.eval_statement(true_body));
                } else {
                    flow!(self.eval_statement(false_body));
                }
                Ok(StatementControlFlow::Continue)
            }
            ast::Statement::While {
                condition, body, ..
            } => {
                while self.eval_expression(condition)?.to_bool() {
                    flow!(self.eval_statement(body));
                }
                Ok(StatementControlFlow::Continue)
            }
            ast::Statement::Print { expression, .. } => {
                let print_value = self.eval_expression(expression)?;
                writeln!(self.stdout, "{}", self.value_to_str(print_value))
                    .expect("Failed to write to stdout");
                Ok(StatementControlFlow::Continue)
            }
            ast::Statement::Return { expression, .. } => {
                let ret_value = expression
                    .as_ref()
                    .map(|expr| self.eval_expression(&expr))
                    .transpose()?
                    .unwrap_or(Value::Nil);
                Ok(StatementControlFlow::Return(ret_value))
            }
            ast::Statement::Expression { expression, .. } => {
                if let Some(expr) = expression {
                    self.eval_expression(expr)?;
                }
                Ok(StatementControlFlow::Continue)
            }
        }
    }

    pub fn eval_expression(&mut self, expr: &ast::Expression) -> Result<Value, EvalError> {
        match expr {
            ast::Expression::AssignExpression(assign) => self.eval_assign_expression(assign),
            ast::Expression::LazyLogical(inner) => self.eval_lazyop_expression(inner),
            ast::Expression::Binary(inner) => self.eval_binop_expression(inner),
            ast::Expression::Unary(inner) => self.eval_unaryop_expression(inner),
            ast::Expression::Call(inner) => self.eval_call_expression(inner),
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

    fn eval_assign_expression(&mut self, expr: &ast::AssignExpression) -> Result<Value, EvalError> {
        let rhs = self.eval_expression(&expr.rhs)?;

        match &*expr.lhs {
            ast::AssignExpressionLhs::Identifier(ident) => {
                if !self.current_env.set_variable(&ident.identifier, rhs) {
                    Err(EvalError {
                        msg: format!(
                            "Variable `{}` is not defined",
                            self.context.resolve_str_symbol(ident.identifier)
                        )
                        .into(),
                        span: ident.span,
                    })
                } else {
                    Ok(rhs)
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

    fn eval_call_expression(&mut self, call: &ast::CallExpression) -> Result<Value, EvalError> {
        let function = self.eval_expression(&call.function)?;

        match function {
            Value::FunctionRef(index) => {
                let args = call
                    .arguments
                    .iter()
                    .map(|arg| self.eval_expression(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                let function = &self.functions[index];
                if function.parameter_count() != args.len() {
                    return Err(EvalError {
                        msg: format!(
                            "Mismatch in argument count, expected {} and got {} arguments",
                            function.parameter_count(),
                            args.len()
                        )
                        .into(),
                        span: call.span(),
                    });
                }

                let body = function.body.clone();

                let new_env = Environment::new();
                for (param, arg) in function.parameters.iter().zip(args.iter()) {
                    new_env.define_variable(*param, *arg);
                }

                self.eval_with_env(new_env, |evaluator| evaluator.eval_statement(&body))?;

                Ok(Value::Nil)
            }
            _ => Err(EvalError {
                msg: "Called expression must be a function".into(),
                span: call.function.span(),
            }),
        }
    }
}

#[derive(Debug)]
pub enum StatementControlFlow {
    Continue,
    Return(Value),
}
