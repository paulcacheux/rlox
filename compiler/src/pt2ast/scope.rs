use std::collections::HashSet;

use string_interner::DefaultSymbol;

#[derive(Debug, Default)]
pub struct Scopes {
    stacks: Vec<HashSet<DefaultSymbol>>,
}

impl Scopes {
    pub fn begin_scope(&mut self) {
        self.stacks.push(HashSet::default());
    }

    pub fn end_scope(&mut self) {
        self.stacks.pop().expect("Failed to end scope");
    }

    pub fn define_identifier(&mut self, sym: DefaultSymbol) -> bool {
        let top_scope = self
            .stacks
            .last_mut()
            .expect("Failed to get top stack when defining an identifier");

        top_scope.insert(sym)
    }
}
