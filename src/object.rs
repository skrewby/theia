use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    ast::{Expression, Statement},
    environment::Environment,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Float(f64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Error(String),
    Function(FunctionObject),
}

#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub parameters: Vec<Expression>,
    pub body: Box<Statement>,
    pub env: Rc<RefCell<Environment>>,
}

impl PartialEq for FunctionObject {
    fn eq(&self, other: &Self) -> bool {
        self.parameters == other.parameters && self.body == other.body
    }
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Int(val) => val.to_string(),
            Object::Float(val) => val.to_string(),
            Object::Boolean(val) => val.to_string(),
            Object::Null => "null".to_string(),
            Object::Error(val) => val.clone(),
            Object::Function(_) => "function".to_owned(),
            Object::Return(ret) => {
                if matches!(**ret, Object::Return(_)) {
                    "Infinite loop".to_string()
                } else {
                    ret.inspect()
                }
            }
        }
    }

    pub fn is_null(&self) -> bool {
        *self == Object::Null
    }

    pub fn bool_value(&self) -> bool {
        match self {
            Object::Int(val) => *val != 0,
            Object::Float(val) => *val != 0.0,
            Object::Boolean(val) => *val,
            Object::Null => false,
            Object::Error(_) => false,
            Object::Function(_) => true,
            Object::Return(ret) => {
                if matches!(**ret, Object::Return(_)) {
                    false
                } else {
                    ret.bool_value()
                }
            }
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Object::Int(val) => *val == 0,
            Object::Float(val) => *val == 0.0,
            _ => false,
        }
    }
}
