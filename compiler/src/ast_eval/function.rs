use string_interner::DefaultSymbol;

use crate::ast;

#[derive(Debug)]
pub struct FunctionEvaluator {
    pub parameters: Vec<DefaultSymbol>,
    pub body: Box<ast::Statement>,
}

impl FunctionEvaluator {
    pub fn parameter_count(&self) -> usize {
        self.parameters.len()
    }
}
