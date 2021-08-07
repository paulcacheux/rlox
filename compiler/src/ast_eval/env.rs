use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};
use string_interner::DefaultSymbol;

use super::Value;

#[derive(Debug, Default)]
pub struct Environment {
    parent: Option<Arc<Environment>>,
    values: Mutex<HashMap<DefaultSymbol, Value>>,
}

impl Environment {
    pub fn new() -> Arc<Self> {
        Arc::new(Environment::default())
    }

    pub fn with_parent(parent: Arc<Environment>) -> Arc<Self> {
        Arc::new(Environment {
            parent: Some(parent),
            values: Mutex::new(HashMap::default()),
        })
    }

    pub fn into_parent(self: Arc<Self>) -> Option<Arc<Self>> {
        self.parent.clone()
    }

    pub fn define_variable(self: &Arc<Self>, name: DefaultSymbol, value: Value) {
        let mut values = self.values.lock().expect("Failed to lock env");
        values.insert(name, value);
    }

    pub fn set_variable(self: &Arc<Self>, name: &DefaultSymbol, value: Value) -> bool {
        let mut values = self.values.lock().expect("Failed to lock env");

        if let Some(entry) = values.get_mut(&name) {
            *entry = value;
            true
        } else if let Some(parent) = &self.parent {
            parent.set_variable(name, value)
        } else {
            false
        }
    }

    pub fn get_value(self: &Arc<Self>, name: &DefaultSymbol) -> Option<Value> {
        let values = self.values.lock().expect("Failed to lock env");
        if let Some(&value) = values.get(name) {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.get_value(name)
        } else {
            None
        }
    }
}
