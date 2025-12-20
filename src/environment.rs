use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::Object;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn enclosed(outer: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => match &self.outer {
                Some(outer) => outer.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }
}
