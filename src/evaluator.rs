use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    ast::{
        CallExpression, Expression, FunctionExpression, IfExpression, IndexExpression,
        InfixExpression, PrefixExpression, Statement, VarStatement, WhileExpression,
    },
    builtin::{call_builtin, get_built_in},
    environment::Environment,
    object::{FunctionObject, Object},
    token::TokenType,
};

pub fn evaluate(stmt: &Statement, env: Rc<RefCell<Environment>>) -> Object {
    eval_statement(stmt, env)
}

fn eval_expression(expr: &Expression, env: Rc<RefCell<Environment>>) -> Object {
    match expr {
        Expression::Int(val) => Object::Int(*val),
        Expression::Float(val) => Object::Float(*val),
        Expression::Str(val) => Object::Str(val.clone()),
        Expression::Boolean(val) => Object::Boolean(*val),
        Expression::Prefix(prefix) => eval_prefix(prefix, env),
        Expression::Infix(infix) => eval_infix(infix, env),
        Expression::If(if_expression) => eval_if(if_expression, env),
        Expression::While(while_expression) => eval_while(while_expression, env),
        Expression::Identifier(name) => eval_identifier(name, env),
        Expression::Function(function) => eval_function(function, env),
        Expression::Call(call) => eval_call(call, env),
        Expression::Array(expressions) => eval_array(expressions, env),
        Expression::Index(index_expression) => eval_index(index_expression, env),
    }
}

fn eval_statement(stmt: &Statement, env: Rc<RefCell<Environment>>) -> Object {
    match stmt {
        Statement::Expression(expr) => eval_expression(expr, env),
        Statement::Return(expr) => Object::Return(Box::new(eval_expression(expr, Rc::clone(&env)))),
        Statement::Break(expr) => Object::Break(Box::new(eval_expression(expr, Rc::clone(&env)))),
        Statement::Program(statements) => eval_statements(statements, env),
        Statement::Block(statements) => eval_block(statements, env),
        Statement::Var(statement) => eval_var(statement, env),
    }
}

fn eval_prefix(prefix: &PrefixExpression, env: Rc<RefCell<Environment>>) -> Object {
    match prefix.operator.token_type {
        TokenType::Bang => eval_prefix_bang(&prefix.right, env),
        TokenType::Minus => eval_prefix_negate(&prefix.right, env),
        _ => Object::Null,
    }
}

fn eval_infix(infix: &InfixExpression, env: Rc<RefCell<Environment>>) -> Object {
    match infix.operator.token_type {
        TokenType::Plus | TokenType::Minus => eval_sum(infix, env),
        TokenType::Asterisk => eval_product(infix, env),
        TokenType::Slash => eval_division(infix, env),
        TokenType::Equal => eval_equality(infix, env),
        TokenType::NotEqual => eval_inequality(infix, env),
        TokenType::GreaterThan => eval_gt(infix, env),
        TokenType::LessThan => eval_lt(infix, env),
        _ => Object::Null,
    }
}

fn eval_identifier(name: &str, env: Rc<RefCell<Environment>>) -> Object {
    match env.borrow().get(name) {
        Some(obj) => obj,
        None => match get_built_in(name) {
            Some(function) => Object::BuiltIn(function),
            None => Object::Error(format!("Identifier not found: {:?}", name)),
        },
    }
}

fn eval_index(index_expression: &IndexExpression, env: Rc<RefCell<Environment>>) -> Object {
    let left = eval_expression(&index_expression.left, Rc::clone(&env));
    if matches!(left, Object::Error(_)) {
        return left;
    }

    let index = eval_expression(&index_expression.index, Rc::clone(&env));
    if matches!(index, Object::Error(_)) {
        return index;
    }

    eval_index_expression(left, index)
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    let Object::Int(i) = index else {
        return Object::Error(format!(
            "Only able to index with integer value. Attempted with {:?}",
            index
        ));
    };
    if i < 0 {
        return Object::Error(format!(
            "Only able to index with positive integer values. Attempted with {:?}",
            index
        ));
    }

    match left {
        Object::Array(arr) => eval_array_index(arr, i),
        _ => Object::Error(format!(
            "Only able to index arrays. Attempted with {:?}",
            left
        )),
    }
}

fn eval_array_index(array: Vec<Object>, index: i64) -> Object {
    if index as usize >= array.len() {
        return Object::Null;
    }
    return array[index as usize].clone();
}

fn eval_array(expressions: &Vec<Expression>, env: Rc<RefCell<Environment>>) -> Object {
    let elements = eval_arg_expressions(expressions, env);
    if elements.len() == 1 && matches!(elements[0], Object::Error(_)) {
        return elements[0].clone();
    }

    Object::Array(elements)
}

fn eval_call(call: &CallExpression, env: Rc<RefCell<Environment>>) -> Object {
    let function = eval_expression(&call.function, Rc::clone(&env));
    if matches!(function, Object::Error(_)) {
        return function;
    }

    let args = eval_arg_expressions(&call.args, env);
    if args.len() == 1 && matches!(args[0], Object::Error(_)) {
        return args[0].clone();
    }

    apply_function(&function, &args)
}

fn apply_function(function: &Object, args: &Vec<Object>) -> Object {
    let func = match function {
        Object::Function(f) => f,
        Object::BuiltIn(f) => return call_builtin(f, args),
        _ => return Object::Error("not a function".to_string()),
    };

    let scoped_env = Rc::new(RefCell::new(Environment::enclosed(Rc::clone(&func.env))));

    for (i, param) in func.parameters.iter().enumerate() {
        if let Expression::Identifier(name) = param {
            scoped_env.borrow_mut().set(name.clone(), args[i].clone());
        }
    }

    let result = eval_statement(&func.body, scoped_env);
    match result {
        Object::Return(val) => *val,
        _ => result,
    }
}

fn eval_arg_expressions(args: &Vec<Expression>, env: Rc<RefCell<Environment>>) -> Vec<Object> {
    let mut objects = Vec::new();

    for expr in args {
        let evaluated = eval_expression(expr, Rc::clone(&env));
        if matches!(evaluated, Object::Error(_)) {
            return vec![evaluated];
        }
        objects.push(evaluated);
    }

    objects
}

fn eval_statements(statements: &Vec<Statement>, env: Rc<RefCell<Environment>>) -> Object {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement, Rc::clone(&env));
        if let Object::Return(val) = result {
            return *val;
        }
        if matches!(result, Object::Return(_)) {
            return result;
        }
    }
    result
}

fn eval_block(statements: &Vec<Statement>, env: Rc<RefCell<Environment>>) -> Object {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement, Rc::clone(&env));
        if matches!(
            result,
            Object::Return(_) | Object::Break(_) | Object::Error(_)
        ) {
            return result;
        }
    }
    result
}

fn eval_prefix_bang(right: &Box<Expression>, env: Rc<RefCell<Environment>>) -> Object {
    let obj = eval_expression(right, env);
    if obj.is_null() {
        return Object::Null;
    }

    Object::Boolean(!obj.bool_value())
}

fn eval_prefix_negate(right: &Box<Expression>, env: Rc<RefCell<Environment>>) -> Object {
    match eval_expression(right, env) {
        Object::Int(val) => Object::Int(-val),
        Object::Float(val) => Object::Float(-val),
        _ => Object::Null,
    }
}

fn eval_if(if_expression: &IfExpression, env: Rc<RefCell<Environment>>) -> Object {
    let condition = eval_expression(&if_expression.condition, Rc::clone(&env));
    if condition.bool_value() {
        return eval_statement(&if_expression.consequence, Rc::clone(&env));
    }

    if let Some(alt) = &if_expression.alternative {
        eval_statement(alt, env)
    } else {
        Object::Null
    }
}

fn eval_while(while_expression: &WhileExpression, env: Rc<RefCell<Environment>>) -> Object {
    let mut last_result = Object::Null;

    loop {
        let condition = eval_expression(&while_expression.condition, Rc::clone(&env));
        if matches!(condition, Object::Error(_)) {
            return condition;
        }

        if !condition.bool_value() {
            return last_result;
        }

        let result = eval_statement(&while_expression.body, Rc::clone(&env));

        if let Object::Break(val) = result {
            return *val;
        }

        if matches!(result, Object::Return(_) | Object::Error(_)) {
            return result;
        }

        last_result = result;
    }
}

fn eval_sum(infix: &InfixExpression, env: Rc<RefCell<Environment>>) -> Object {
    let left = eval_expression(&infix.left, Rc::clone(&env));
    let right = eval_expression(&infix.right, env);

    match infix.operator.token_type == TokenType::Plus {
        true => eval_addition(&left, &right),
        false => eval_subtraction(&left, &right),
    }
}

fn eval_addition(left: &Object, right: &Object) -> Object {
    match (&left, &right) {
        (Object::Int(_) | Object::Float(_), Object::Int(_) | Object::Float(_)) => {
            eval_sum_numbers(&left, &right, true)
        }
        _ if (matches!(&left, Object::Array(_)) || matches!(&right, Object::Array(_))) => {
            eval_concat_array(&left, &right)
        }
        _ if (matches!(&left, Object::Str(_)) || matches!(&right, Object::Str(_))) => {
            Object::Str(format!("{}{}", left.inspect(), right.inspect()))
        }
        _ => Object::Error(format!(
            "Type mismatch: {} + {}",
            left.inspect(),
            right.inspect()
        )),
    }
}

fn eval_subtraction(left: &Object, right: &Object) -> Object {
    match (&left, &right) {
        (Object::Int(_) | Object::Float(_), Object::Int(_) | Object::Float(_)) => {
            eval_sum_numbers(&left, &right, false)
        }
        _ => Object::Error(format!(
            "Type mismatch: {} - {}",
            left.inspect(),
            right.inspect()
        )),
    }
}

fn eval_sum_numbers(left: &Object, right: &Object, is_addition: bool) -> Object {
    match (left, right) {
        (Object::Int(l), Object::Int(r)) => Object::Int(if is_addition { l + r } else { l - r }),
        (Object::Float(l), Object::Float(r)) => {
            Object::Float(if is_addition { l + r } else { l - r })
        }
        (Object::Int(l), Object::Float(r)) => Object::Float(if is_addition {
            *l as f64 + r
        } else {
            *l as f64 - r
        }),
        (Object::Float(l), Object::Int(r)) => Object::Float(if is_addition {
            l + *r as f64
        } else {
            l - *r as f64
        }),
        _ => unreachable!(),
    }
}

fn eval_concat_array(left: &Object, right: &Object) -> Object {
    match (left, right) {
        (Object::Array(l), Object::Array(r)) => {
            let mut result = l.clone();
            result.extend(r.clone());
            Object::Array(result)
        }
        (Object::Array(l), r) => {
            let mut result = l.clone();
            result.push(r.clone());
            Object::Array(result)
        }
        (l, Object::Array(r)) => {
            let mut result = vec![l.clone()];
            result.extend(r.clone());
            Object::Array(result)
        }
        _ => unreachable!(),
    }
}

fn eval_product(infix: &InfixExpression, env: Rc<RefCell<Environment>>) -> Object {
    let left = eval_expression(&infix.left, Rc::clone(&env));
    let right = eval_expression(&infix.right, env);

    match (&left, &right) {
        (Object::Int(l), Object::Int(r)) => Object::Int(*l * *r),
        (Object::Float(l), Object::Float(r)) => Object::Float(*l * *r),
        (Object::Int(l), Object::Float(r)) => Object::Float(*l as f64 * *r),
        (Object::Float(l), Object::Int(r)) => Object::Float(*l * *r as f64),
        _ => Object::Error(
            format!("Type mismatch: {} * {}", left.inspect(), right.inspect()).to_owned(),
        ),
    }
}

fn eval_division(infix: &InfixExpression, env: Rc<RefCell<Environment>>) -> Object {
    let left = eval_expression(&infix.left, Rc::clone(&env));
    let right = eval_expression(&infix.right, env);

    if right.is_zero() {
        return Object::Error(
            format!(
                "Unable to divide by zero: {} / {}",
                left.inspect(),
                right.inspect()
            )
            .to_owned(),
        );
    }

    match (&left, &right) {
        (Object::Int(l), Object::Int(r)) => Object::Int(*l / *r),
        (Object::Float(l), Object::Float(r)) => Object::Float(*l / *r),
        (Object::Int(l), Object::Float(r)) => Object::Float(*l as f64 / *r),
        (Object::Float(l), Object::Int(r)) => Object::Float(*l / *r as f64),
        _ => Object::Error(
            format!("Type mismatch: {} / {}", left.inspect(), right.inspect()).to_owned(),
        ),
    }
}

fn eval_equality(infix: &InfixExpression, env: Rc<RefCell<Environment>>) -> Object {
    let left = eval_expression(&infix.left, Rc::clone(&env));
    let right = eval_expression(&infix.right, env);

    match (&left, &right) {
        (Object::Int(l), Object::Int(r)) => Object::Boolean(*l == *r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(*l == *r),
        (Object::Int(l), Object::Float(r)) => Object::Boolean(*l as f64 == *r),
        (Object::Float(l), Object::Int(r)) => Object::Boolean(*l == *r as f64),
        _ => Object::Error(
            format!("Type mismatch: {} == {}", left.inspect(), right.inspect()).to_owned(),
        ),
    }
}

fn eval_inequality(infix: &InfixExpression, env: Rc<RefCell<Environment>>) -> Object {
    let left = eval_expression(&infix.left, Rc::clone(&env));
    let right = eval_expression(&infix.right, env);

    match (&left, &right) {
        (Object::Int(l), Object::Int(r)) => Object::Boolean(*l != *r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(*l != *r),
        (Object::Int(l), Object::Float(r)) => Object::Boolean(*l as f64 != *r),
        (Object::Float(l), Object::Int(r)) => Object::Boolean(*l != *r as f64),
        _ => Object::Error(
            format!("Type mismatch: {} != {}", left.inspect(), right.inspect()).to_owned(),
        ),
    }
}

fn eval_gt(infix: &InfixExpression, env: Rc<RefCell<Environment>>) -> Object {
    let left = eval_expression(&infix.left, Rc::clone(&env));
    let right = eval_expression(&infix.right, env);

    match (&left, &right) {
        (Object::Int(l), Object::Int(r)) => Object::Boolean(*l > *r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(*l > *r),
        (Object::Int(l), Object::Float(r)) => Object::Boolean(*l as f64 > *r),
        (Object::Float(l), Object::Int(r)) => Object::Boolean(*l > *r as f64),
        _ => Object::Error(
            format!("Type mismatch: {} > {}", left.inspect(), right.inspect()).to_owned(),
        ),
    }
}

fn eval_lt(infix: &InfixExpression, env: Rc<RefCell<Environment>>) -> Object {
    let left = eval_expression(&infix.left, Rc::clone(&env));
    let right = eval_expression(&infix.right, env);

    match (&left, &right) {
        (Object::Int(l), Object::Int(r)) => Object::Boolean(*l < *r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(*l < *r),
        (Object::Int(l), Object::Float(r)) => Object::Boolean((*l as f64) < *r),
        (Object::Float(l), Object::Int(r)) => Object::Boolean(*l < *r as f64),
        _ => Object::Error(
            format!("Type mismatch: {} < {}", left.inspect(), right.inspect()).to_owned(),
        ),
    }
}

fn eval_var(var: &VarStatement, env: Rc<RefCell<Environment>>) -> Object {
    let result = eval_expression(&var.expression, Rc::clone(&env));
    if matches!(result, Object::Error(_)) {
        return result;
    }

    let TokenType::Identifier(name) = &var.identifier.token_type else {
        return Object::Error(format!("Must bind an expression to an identifier"));
    };

    env.borrow_mut().set(name.clone(), result.clone());
    result
}

fn eval_function(function: &FunctionExpression, env: Rc<RefCell<Environment>>) -> Object {
    Object::Function(FunctionObject {
        parameters: function.parameters.clone(),
        body: function.body.clone(),
        env: env,
    })
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn object_number() {
        let input = "
            3;
            100;
            10_000;
            3.145;
            123.456;
        ";
        let expected = vec![
            Object::Int(3),
            Object::Int(100),
            Object::Int(10000),
            Object::Float(3.145),
            Object::Float(123.456),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn object_boolean() {
        let input = "
            true;
            false;
            2 == 2;
            10 != 3;
            10 != 10;
            5 * 2 == 14 - 4;
            52 * 23 + 2 != 10;
            5 * 2 > 8;
            1 * 5 < 30 / 2;
            10 + 2 * 3 > 50;
        ";
        let expected = vec![
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(true),
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(true),
            Object::Boolean(true),
            Object::Boolean(true),
            Object::Boolean(true),
            Object::Boolean(false),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn object_bang() {
        let input = "
            !true;
            !false;
            !!false;
            !5;
            !!5;
        ";
        let expected = vec![
            Object::Boolean(false),
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Boolean(false),
            Object::Boolean(true),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn object_negate() {
        let input = "
            -5;
            -10;
            10;
            43.33;
            -130.279;
        ";
        let expected = vec![
            Object::Int(-5),
            Object::Int(-10),
            Object::Int(10),
            Object::Float(43.33),
            Object::Float(-130.279),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn infix() {
        let input = "
            3 * 2;
            4 + 5 + 1 + 5 * 2;
            60 / 2 + 50;
            (3 + 2) * 2;
            (80 + 120) / 20;
            10 * -2;
            10 / 0;
            10 / -5;
        ";
        let expected = vec![
            Object::Int(6),
            Object::Int(20),
            Object::Int(80),
            Object::Int(10),
            Object::Int(10),
            Object::Int(-20),
            Object::Error("Unable to divide by zero: 10 / 0".to_owned()),
            Object::Int(-2),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn if_expressions() {
        let input = "
            if true { 6 }
            if false { 20 }
            if 10 { 20 }
            if 1 < 2 { 30 }
            if 1 > 2 { 30 * 10 } else { 40 * 20 }
            if 1 > 2 { 30 }
        ";
        let expected = vec![
            Object::Int(6),
            Object::Null,
            Object::Int(20),
            Object::Int(30),
            Object::Int(800),
            Object::Null,
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn return_expressions() {
        let input = "
            return 10;
            return 20 * 5;
            return 100 == 20 * 5;
            9; return 2 * 5; 9;
        ";
        let expected = vec![
            Object::Return(Box::new(Object::Int(10))),
            Object::Return(Box::new(Object::Int(100))),
            Object::Return(Box::new(Object::Boolean(true))),
            Object::Int(9),
            Object::Return(Box::new(Object::Int(10))),
            Object::Int(9),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn nested_return() {
        let input = "
            if (true) {
                if (true) {
                    return 100;
                }
                return 20;
            }
        ";
        let expected = vec![Object::Return(Box::new(Object::Int(100)))];
        check_matches(input, &expected);
    }

    #[test]
    fn assign() {
        let input = "
            var foo = 5; foo;
            var bar = 20; var foobar = bar; foobar;
        ";
        let expected = vec![
            Object::Int(5),
            Object::Int(5),
            Object::Int(20),
            Object::Int(20),
            Object::Int(20),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn functions() {
        let input = "
            fn(x) { x + 2; }
        ";
        let expected = vec![Object::Function(FunctionObject {
            parameters: vec![Expression::Identifier("x".to_owned())],
            body: Box::new(Statement::Block(vec![Statement::Expression(
                Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Identifier("x".to_owned())),
                    operator: crate::token::Token {
                        token_type: TokenType::Plus,
                    },
                    right: Box::new(Expression::Int(2)),
                }),
            )])),
            env: Rc::new(RefCell::new(Environment::new())),
        })];
        check_matches(input, &expected);
    }

    #[test]
    fn closures() {
        let input = "
            var adder = fn(x) {
                var foo = 2;
                fn(y) { x + y }
            };
            var add_two = adder(2);
            add_two(2);
        ";

        let statements = setup_test(input);
        let env = Rc::new(RefCell::new(Environment::new()));
        for (i, statement) in statements.iter().enumerate() {
            let obj = evaluate(statement, Rc::clone(&env));
            if i == statements.len() - 1 {
                assert_eq!(obj, Object::Int(4));
            }
        }
    }

    #[test]
    fn strings() {
        let input = "
            \"Goodbye\"
            var foo = \"Hello\"
            foo
            var bar = \"World\"
            foo + \" \" + bar
            foo + 2
        ";
        let expected = vec![
            Object::Str("Goodbye".to_owned()),
            Object::Str("Hello".to_owned()),
            Object::Str("Hello".to_owned()),
            Object::Str("World".to_owned()),
            Object::Str("Hello World".to_owned()),
            Object::Str("Hello2".to_owned()),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn builtin_len() {
        let input = "
            len(\"Hello\")
        ";
        let expected = vec![Object::Int(5)];
        check_matches(input, &expected);
    }

    #[test]
    fn builtin_first() {
        let input = "
            var foo = [1, 2, 3]
            first(foo)
        ";
        let expected = vec![
            Object::Array(vec![Object::Int(1), Object::Int(2), Object::Int(3)]),
            Object::Int(1),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn builtin_last() {
        let input = "
            var foo = [1, 2, 3, 4, 5]
            last(foo)
        ";
        let expected = vec![
            Object::Array(vec![
                Object::Int(1),
                Object::Int(2),
                Object::Int(3),
                Object::Int(4),
                Object::Int(5),
            ]),
            Object::Int(5),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn builtin_tail() {
        let input = "
            var foo = [1, 2, 3, 4, 5]
            tail(foo, 3)
        ";
        let expected = vec![
            Object::Array(vec![
                Object::Int(1),
                Object::Int(2),
                Object::Int(3),
                Object::Int(4),
                Object::Int(5),
            ]),
            Object::Array(vec![Object::Int(3), Object::Int(4), Object::Int(5)]),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn while_loop() {
        let input = "
            var i = 0
            while i < 3 {
                var i = i + 1
            }

            var i = 0
            while i < 10 {
                var i = i + 1
                if i == 6 {
                    break i
                }
            }

            var i = 0
            while i < 10 {
                if i == 5 {
                    break i
                }
                var i = i + 1
            }
        ";
        let expected = vec![
            Object::Int(0),
            Object::Int(3),
            Object::Int(0),
            Object::Int(6),
            Object::Int(0),
            Object::Int(5),
        ];
        check_matches(input, &expected);
    }

    #[test]
    fn arrays() {
        let input = "
            var foo = [1, 2 * 2]
            foo[1]
            len(foo)
            var bar = foo + [5, 6]
            bar + 7
        ";
        let expected = vec![
            Object::Array(vec![Object::Int(1), Object::Int(4)]),
            Object::Int(4),
            Object::Int(2),
            Object::Array(vec![
                Object::Int(1),
                Object::Int(4),
                Object::Int(5),
                Object::Int(6),
            ]),
            Object::Array(vec![
                Object::Int(1),
                Object::Int(4),
                Object::Int(5),
                Object::Int(6),
                Object::Int(7),
            ]),
        ];
        check_matches(input, &expected);
    }

    fn check_matches(input: &str, expected: &Vec<Object>) {
        let statements = setup_test(input);
        let env = Rc::new(RefCell::new(Environment::new()));
        for (i, statement) in statements.iter().enumerate() {
            let obj = evaluate(statement, Rc::clone(&env));
            assert_eq!(obj, expected[i]);
        }
    }

    fn setup_test(input: &str) -> Vec<Statement> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parsing_errors(&parser);

        let Statement::Program(statements) = program else {
            panic!("parse_program needs to return a Statement::Program");
        };

        statements
    }

    fn check_parsing_errors(parser: &Parser) {
        if !parser.errors.is_empty() {
            for e in &parser.errors {
                println!("{}", e);
            }
            panic!("Parser had errors");
        }
    }
}
