use std::collections::HashSet;

use string_interner::DefaultSymbol;

#[derive(Debug, Default)]
pub struct Scopes {
    stacks: Vec<HashSet<DefaultSymbol>>,
    current_var_decl: Option<DefaultSymbol>,
}

impl Scopes {
    pub fn begin_scope(&mut self) {
        self.stacks.push(HashSet::default());
    }

    pub fn end_scope(&mut self) {
        self.stacks.pop().expect("Failed to end scope");
    }

    pub fn is_at_global_scope(&self) -> bool {
        self.stacks.len() <= 1
    }

    pub fn define_identifier(&mut self, sym: DefaultSymbol) -> bool {
        let top_scope = self
            .stacks
            .last_mut()
            .expect("Failed to get top stack when defining an identifier");

        let ret_value = top_scope.insert(sym);
        if self.is_at_global_scope() {
            true
        } else {
            ret_value
        }
    }

    pub fn is_current_var_decl(&self, sym: DefaultSymbol) -> bool {
        self.current_var_decl == Some(sym)
    }

    pub fn set_current_var_decl(&mut self, sym: DefaultSymbol) {
        self.current_var_decl = Some(sym);
    }

    pub fn clear_current_var_decl(&mut self) {
        self.current_var_decl = None
    }
}
