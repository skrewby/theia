use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub struct Symbol {
    pub index: u16,
}

pub struct SymbolTable {
    string_store: HashMap<String, u16>,
    symbol_store: HashMap<u16, Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            string_store: HashMap::new(),
            symbol_store: HashMap::new(),
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

    pub fn define(&mut self, name: &str) -> u16 {
        let index = self.intern(name);

        let symbol = Symbol { index };

        self.symbol_store.insert(index, symbol);

        index
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        let name_index = self.string_store.get(name)?;
        self.symbol_store.get(name_index).copied()
    }
}
