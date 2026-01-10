use std::collections::HashMap;

use crate::builtin::BuiltInFunction;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
}

#[derive(Debug, Clone, Copy)]
pub struct Symbol {
    pub scope: SymbolScope,
    pub index: u16,
}

#[derive(Clone)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,
    string_store: HashMap<String, u16>,
    symbol_store: HashMap<u16, Symbol>,
    pub num_definitions: usize,
    builtin_store: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            outer: None,
            string_store: HashMap::new(),
            symbol_store: HashMap::new(),
            num_definitions: 0,
            builtin_store: HashMap::new(),
        }
    }

    pub fn new_enclosed(outer_table: SymbolTable) -> SymbolTable {
        SymbolTable {
            outer: Some(Box::new(outer_table)),
            string_store: HashMap::new(),
            symbol_store: HashMap::new(),
            num_definitions: 0,
            builtin_store: HashMap::new(),
        }
    }

    fn intern(&mut self, name: &str) -> u16 {
        if let Some(&index) = self.string_store.get(name) {
            return index;
        }

        let index = self.symbol_store.len() as u16;
        self.string_store.insert(name.to_string(), index);

        index
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let index = self.intern(name);

        let scope = if self.outer.is_none() {
            SymbolScope::Global
        } else {
            SymbolScope::Local
        };
        let symbol = Symbol { index, scope };

        self.symbol_store.insert(index, symbol);
        self.num_definitions += 1;

        symbol
    }

    pub fn register_builtins(&mut self) -> HashMap<String, Symbol> {
        let store = HashMap::new();

        for (index, builtin) in BuiltInFunction::ITERATE.iter().enumerate() {
            let symbol = Symbol {
                scope: SymbolScope::Builtin,
                index: index as u16,
            };
            self.builtin_store.insert(builtin.name().to_owned(), symbol);
        }

        store
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = self.builtin_store.get(name) {
            return Some(*symbol);
        }

        if let Some(&name_index) = self.string_store.get(name) {
            if let Some(symbol) = self.symbol_store.get(&name_index) {
                return Some(*symbol);
            }
        }

        if let Some(outer) = &self.outer {
            return outer.resolve(name);
        }

        None
    }

    pub fn into_outer(self) -> SymbolTable {
        *self.outer.expect("No outer scope present on return")
    }
}
