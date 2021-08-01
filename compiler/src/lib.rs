use std::sync::Mutex;

use string_interner::{DefaultSymbol, StringInterner};

pub mod ast;
pub mod lexer;
pub mod parse_tree;
pub mod parser;
pub mod tree_common;

#[derive(Debug, Default)]
pub struct CompilationContext {
    pub interner: Mutex<StringInterner>,
}

impl CompilationContext {
    pub fn intern_string(&self, string: String) -> DefaultSymbol {
        let mut interner = self.interner.lock().expect("Failed to lock interner");
        interner.get_or_intern(string)
    }

    pub fn resolve_str_symbol(&self, symbol: DefaultSymbol) -> String {
        let interner = self.interner.lock().expect("Failed to lock interner");
        interner
            .resolve(symbol)
            .map(|s| s.to_owned())
            .expect("Resolve unexisting symbol")
    }
}
