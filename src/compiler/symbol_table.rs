use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SymbolScope {
    Global,
    Local,
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
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            outer: None,
            string_store: HashMap::new(),
            symbol_store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn new_enclosed(outer_table: SymbolTable) -> SymbolTable {
        SymbolTable {
            outer: Some(Box::new(outer_table)),
            string_store: HashMap::new(),
            symbol_store: HashMap::new(),
            num_definitions: 0,
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

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
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

