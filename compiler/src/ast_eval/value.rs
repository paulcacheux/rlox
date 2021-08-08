use string_interner::DefaultSymbol;

use super::builtin::BuiltinFunction;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Bool(bool),
    String(DefaultSymbol),
    FunctionRef(usize),
    BuiltinFunction(BuiltinFunction),
}

impl Value {
    pub fn to_bool(self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }
}
