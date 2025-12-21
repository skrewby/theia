use crate::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltInFunction {
    Len,
    Print,
    First,
    Last,
    Tail,
}

pub fn get_built_in(name: &str) -> Option<BuiltInFunction> {
    match name {
        "len" => Some(BuiltInFunction::Len),
        "print" => Some(BuiltInFunction::Print),
        "first" => Some(BuiltInFunction::First),
        "last" => Some(BuiltInFunction::Last),
        "tail" => Some(BuiltInFunction::Tail),
        _ => None,
    }
}

pub fn call_builtin(function: &BuiltInFunction, args: &Vec<Object>) -> Object {
    match function {
        BuiltInFunction::Len => builtin_len(args),
        BuiltInFunction::Print => builtin_print(args),
        BuiltInFunction::First => builtin_first(args),
        BuiltInFunction::Last => builtin_last(args),
        BuiltInFunction::Tail => builtin_tail(args),
    }
}

fn builtin_len(args: &Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "len: wrong number of arguments, expected 1 but got {}",
            args.len()
        ));
    }
    let length = match &args[0] {
        Object::Str(s) => s.len() as i64,
        Object::Array(arr) => arr.len() as i64,
        _ => return Object::Error(format!("len: argument not supported, got {:?}", args[0])),
    };

    Object::Int(length)
}

fn builtin_first(args: &Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "first: wrong number of arguments, expected 1 but got {}",
            args.len()
        ));
    }

    let objects = match &args[0] {
        Object::Array(arr) => arr,
        _ => return Object::Error(format!("first: argument not supported, got {:?}", args[0])),
    };

    if objects.is_empty() {
        return Object::Null;
    }

    objects[0].clone()
}

fn builtin_tail(args: &Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error(format!(
            "tail: wrong number of arguments, expected 2 but got {}",
            args.len()
        ));
    }

    let objects = match &args[0] {
        Object::Array(arr) => arr,
        _ => return Object::Error(format!("tail: argument not supported, got {:?}", args[0])),
    };
    let num = match &args[1] {
        Object::Int(n) => *n,
        _ => {
            return Object::Error(format!(
                "tail: argument not supported, expected Int, got {:?}",
                args[1]
            ));
        }
    };
    if num <= 0 {
        return Object::Error(format!(
            "tail: argument not support, expected positive, got {}",
            args.len()
        ));
    }
    if objects.is_empty() {
        return Object::Null;
    }

    let start = objects.len().saturating_sub(num as usize);
    Object::Array(objects[start..].to_vec())
}

fn builtin_last(args: &Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "last: wrong number of arguments, expected 1 but got {}",
            args.len()
        ));
    }

    let objects = match &args[0] {
        Object::Array(arr) => arr,
        _ => return Object::Error(format!("last: argument not supported, got {:?}", args[0])),
    };

    match objects.last() {
        Some(o) => o.clone(),
        None => Object::Null,
    }
}

fn builtin_print(args: &Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "print: wrong number of arguments, expected 1 but got {}",
            args.len()
        ));
    }
    let Object::Str(s) = &args[0] else {
        return Object::Error(format!("print: argument not supported, got {:?}", args[0]));
    };

    println!("{}", s);
    Object::Null
}
