use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};
use string_interner::DefaultSymbol;

use super::Value;

#[derive(Debug)]
pub struct Environment {
    globals: Arc<Globals>,
    top_scope: Option<Arc<LocalScope>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            globals: Default::default(),
            top_scope: None,
        }
    }

    pub fn clone_for_function(&self) -> Environment {
        Environment {
            globals: self.globals.clone(),
            top_scope: self
                .top_scope
                .as_ref()
                .map(|ts| Arc::new(ts.clone_for_function())),
        }
    }

    pub fn begin_scope(&mut self) {
        if let Some(ts) = self.top_scope.take() {
            self.top_scope = Some(LocalScope::with_parent(ts));
        } else {
            self.top_scope = Some(LocalScope::new());
        }
    }

    pub fn end_scope(&mut self) {
        if let Some(ts) = self.top_scope.take() {
            self.top_scope = ts.into_parent();
        } else {
            unreachable!()
        }
    }

    pub fn define_variable(&self, name: DefaultSymbol, value: Value) {
        if let Some(ts) = &self.top_scope {
            ts.define_variable(name, value)
        } else {
            self.globals.define_variable(name, value)
        }
    }

    pub fn set_variable(&self, name: &DefaultSymbol, value: Value) -> bool {
        if let Some(ts) = &self.top_scope {
            ts.set_variable(name, value, &self.globals)
        } else {
            self.globals.set_variable(name, value)
        }
    }

    pub fn get_value(&self, name: &DefaultSymbol) -> Option<Value> {
        if let Some(ts) = &self.top_scope {
            ts.get_value(name, &self.globals)
        } else {
            self.globals.get_value(name)
        }
    }
}

#[derive(Debug, Default)]
pub struct Globals {
    values: Mutex<HashMap<DefaultSymbol, Value>>,
}

impl Globals {
    fn define_variable(&self, name: DefaultSymbol, value: Value) {
        let mut values = self.values.lock().expect("Failed to lock env");
        values.insert(name, value);
    }

    fn set_variable(&self, name: &DefaultSymbol, value: Value) -> bool {
        let mut values = self.values.lock().expect("Failed to lock env");

        if let Some(entry) = values.get_mut(name) {
            *entry = value;
            true
        } else {
            false
        }
    }

    fn get_value(&self, name: &DefaultSymbol) -> Option<Value> {
        let values = self.values.lock().expect("Failed to lock env");
        if let Some(&value) = values.get(name) {
            Some(value)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct LocalScope {
    parent: Option<Arc<LocalScope>>,
    values: Mutex<HashMap<DefaultSymbol, Value>>,
}

impl LocalScope {
    fn new() -> Arc<Self> {
        Arc::new(LocalScope {
            parent: None,
            values: Default::default(),
        })
    }

    fn clone_for_function(&self) -> LocalScope {
        let values = self.values.lock().expect("Failed to lock env");
        LocalScope {
            parent: self.parent.clone(),
            values: Mutex::new(values.clone()),
        }
    }

    fn with_parent(parent: Arc<LocalScope>) -> Arc<Self> {
        Arc::new(LocalScope {
            parent: Some(parent),
            values: Mutex::new(HashMap::default()),
        })
    }

    fn into_parent(self: Arc<Self>) -> Option<Arc<Self>> {
        self.parent.clone()
    }

    fn define_variable(&self, name: DefaultSymbol, value: Value) {
        let mut values = self.values.lock().expect("Failed to lock env");
        values.insert(name, value);
    }

    fn set_variable(&self, name: &DefaultSymbol, value: Value, globals: &Globals) -> bool {
        let mut values = self.values.lock().expect("Failed to lock env");

        if let Some(entry) = values.get_mut(name) {
            *entry = value;
            true
        } else if let Some(parent) = &self.parent {
            parent.set_variable(name, value, globals)
        } else {
            globals.set_variable(name, value)
        }
    }

    fn get_value(&self, name: &DefaultSymbol, globals: &Globals) -> Option<Value> {
        let values = self.values.lock().expect("Failed to lock env");
        if let Some(&value) = values.get(name) {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.get_value(name, globals)
        } else {
            globals.get_value(name)
        }
    }
}
