use crate::builtin::BuiltInFunction;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Break(Box<Object>),
    Error(String),
    Function(FunctionObject),
    BuiltIn(BuiltInFunction),
    Array(Vec<Object>),
}

#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub instructions: Vec<u8>,
    pub num_locals: usize,
    pub num_parameters: usize,
}

impl PartialEq for FunctionObject {
    fn eq(&self, other: &Self) -> bool {
        self.instructions == other.instructions
    }
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Int(val) => val.to_string(),
            Object::Float(val) => val.to_string(),
            Object::Str(val) => val.clone(),
            Object::Boolean(val) => val.to_string(),
            Object::Null => "null".to_string(),
            Object::Error(val) => val.clone(),
            Object::Function(_) => "function".to_owned(),
            Object::BuiltIn(function) => format!("Built in function: {:?}", function),
            Object::Array(objects) => {
                let elements: Vec<String> = objects.iter().map(|obj| obj.inspect()).collect();
                format!("[{}]", elements.join(", "))
            }
            Object::Return(ret) => {
                if matches!(**ret, Object::Return(_)) {
                    "Infinite loop".to_string()
                } else {
                    ret.inspect()
                }
            }
            Object::Break(val) => val.inspect(),
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
            Object::BuiltIn(_) => true,
            Object::Str(_) => true,
            Object::Array(objects) => objects.iter().all(|obj| obj.bool_value()),
            Object::Return(ret) => {
                if matches!(**ret, Object::Return(_)) {
                    false
                } else {
                    ret.bool_value()
                }
            }
            Object::Break(val) => val.bool_value(),
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
