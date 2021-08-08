use string_interner::DefaultSymbol;

use crate::ast;

use super::env::Environment;

#[derive(Debug)]
pub struct FunctionEvaluator {
    pub name: DefaultSymbol,
    pub parent_env: Environment,
    pub parameters: Vec<DefaultSymbol>,
    pub body: Box<ast::Statement>,
}

impl FunctionEvaluator {
    pub fn parameter_count(&self) -> usize {
        self.parameters.len()
    }
}
