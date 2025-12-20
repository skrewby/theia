use crate::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltInFunction {
    Len,
    Print,
}

pub fn get_built_in(name: &str) -> Option<BuiltInFunction> {
    match name {
        "len" => Some(BuiltInFunction::Len),
        "print" => Some(BuiltInFunction::Print),
        _ => None,
    }
}

pub fn call_builtin(function: &BuiltInFunction, args: &Vec<Object>) -> Object {
    match function {
        BuiltInFunction::Len => builtin_len(args),
        BuiltInFunction::Print => builtin_print(args),
    }
}

fn builtin_len(args: &Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "len: wrong number of arguments, expected 1 but got {}",
            args.len()
        ));
    }
    let Object::Str(s) = &args[0] else {
        return Object::Error(format!("len: argument not supported, got {:?}", args[0]));
    };

    Object::Int(s.len() as i64)
}

fn builtin_print(args: &Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "len: wrong number of arguments, expected 1 but got {}",
            args.len()
        ));
    }
    let Object::Str(s) = &args[0] else {
        return Object::Error(format!("len: argument not supported, got {:?}", args[0]));
    };

    println!("{}", s);
    Object::Null
}
