use crate::{
    ast::{
        CallExpression, Expression, FunctionExpression, IfExpression, IndexExpression,
        InfixExpression, LetStatement, PrefixExpression, Statement, WhileExpression,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Assign,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            current_token: Token {
                token_type: TokenType::EOF,
            },
            peek_token: Token {
                token_type: TokenType::EOF,
            },
            errors: Vec::new(),
        };
        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn parse_program(&mut self) -> Statement {
        let mut statements = Vec::new();

        while self.current_token.token_type != TokenType::EOF {
            if let Some(statement) = parse_statement(self) {
                statements.push(statement);
            } else {
                let msg = format!("Unexpected token: {:?}", self.current_token);
                self.errors.push(msg);
            }
            self.next_token();
        }

        Statement::Program(statements)
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn print_errors(&self) {
        for err in &self.errors {
            println!("{}", err);
        }
    }

    fn peek_expected(&mut self, expected: TokenType) -> Option<()> {
        if self.peek_token.token_type == expected {
            return Some(());
        }
        let msg = format!(
            "Mismatched tokens: expected {:?} but got {:?}",
            expected, self.peek_token.token_type
        );
        self.errors.push(msg);
        None
    }
}

fn parse_statement(parser: &mut Parser) -> Option<Statement> {
    match parser.current_token.token_type {
        TokenType::Variable => parse_let_statement(parser),
        TokenType::Return => parse_return_statement(parser),
        TokenType::Break => parse_break_statement(parser),
        _ => parse_expression_statement(parser),
    }
}

fn parse_prefix_expression(parser: &mut Parser) -> Option<Expression> {
    match parser.current_token.token_type {
        TokenType::Identifier(_) => parse_identifier(parser),
        TokenType::Int(_) => parse_int(parser),
        TokenType::Float(_) => parse_float(parser),
        TokenType::Bang | TokenType::Minus => parse_prefix(parser),
        TokenType::True | TokenType::False => parse_boolean(parser),
        TokenType::LParen => parse_group(parser),
        TokenType::If => parse_if(parser),
        TokenType::While => parse_while(parser),
        TokenType::Function => parse_fn_literal(parser),
        TokenType::Str(_) => parse_string(parser),
        TokenType::LBracket => parse_array(parser),
        _ => None,
    }
}

fn parse_infix_expression(parser: &mut Parser, left: Expression) -> Option<Expression> {
    match parser.current_token.token_type {
        TokenType::Plus
        | TokenType::Minus
        | TokenType::Assign
        | TokenType::Slash
        | TokenType::Asterisk
        | TokenType::Equal
        | TokenType::NotEqual
        | TokenType::LessThan
        | TokenType::GreaterThan => parse_infix(parser, left),
        TokenType::LParen => parse_call(parser, left),
        TokenType::LBracket => parse_index(parser, left),
        _ => None,
    }
}

fn parse_let_statement(parser: &mut Parser) -> Option<Statement> {
    if !matches!(parser.peek_token.token_type, TokenType::Identifier(_)) {
        let msg = format!(
            "Expected identifier but got {:?}",
            parser.peek_token.token_type
        );
        parser.errors.push(msg);
        return None;
    }
    parser.next_token();
    let identifier = parser.current_token.clone();

    parser.peek_expected(TokenType::Assign)?;
    parser.next_token();
    parser.next_token();

    let expression = parse_expression(parser, Precedence::Lowest)?;

    if matches!(parser.peek_token.token_type, TokenType::Semicolon) {
        parser.next_token();
    }

    Some(Statement::VariableAssign(LetStatement {
        identifier,
        expression,
    }))
}

fn parse_return_statement(parser: &mut Parser) -> Option<Statement> {
    parser.next_token();
    let val = parse_expression(parser, Precedence::Lowest)?;

    if matches!(parser.peek_token.token_type, TokenType::Semicolon) {
        parser.next_token();
    }

    Some(Statement::Return(val))
}

fn parse_break_statement(parser: &mut Parser) -> Option<Statement> {
    parser.next_token();
    let val = parse_expression(parser, Precedence::Lowest)?;

    if matches!(parser.peek_token.token_type, TokenType::Semicolon) {
        parser.next_token();
    }

    Some(Statement::Break(val))
}

fn parse_expression_statement(parser: &mut Parser) -> Option<Statement> {
    let expression = parse_expression(parser, Precedence::Lowest)?;

    if matches!(parser.peek_token.token_type, TokenType::Semicolon) {
        parser.next_token();
    }

    Some(Statement::Expression(expression))
}

fn parse_expression(parser: &mut Parser, precedence: Precedence) -> Option<Expression> {
    let mut left_expression = parse_prefix_expression(parser)?;

    while !matches!(parser.peek_token.token_type, TokenType::Semicolon)
        && precedence < token_precedence(&parser.peek_token.token_type)
    {
        parser.next_token();
        left_expression = parse_infix_expression(parser, left_expression)?;
    }

    Some(left_expression)
}

fn parse_identifier(parser: &mut Parser) -> Option<Expression> {
    let TokenType::Identifier(name) = &parser.current_token.token_type else {
        return None;
    };

    Some(Expression::Identifier(name.clone()))
}

fn parse_int(parser: &mut Parser) -> Option<Expression> {
    let TokenType::Int(val) = parser.current_token.token_type else {
        return None;
    };

    Some(Expression::Int(val))
}

fn parse_float(parser: &mut Parser) -> Option<Expression> {
    let TokenType::Float(val) = parser.current_token.token_type else {
        return None;
    };

    Some(Expression::Float(val))
}

fn parse_string(parser: &mut Parser) -> Option<Expression> {
    let TokenType::Str(val) = &parser.current_token.token_type else {
        return None;
    };

    Some(Expression::Str(val.clone()))
}

fn parse_prefix(parser: &mut Parser) -> Option<Expression> {
    let operator = parser.current_token.clone();
    parser.next_token();
    let right = parse_expression(parser, Precedence::Prefix)?;

    Some(Expression::Prefix(PrefixExpression {
        operator,
        right: Box::new(right),
    }))
}

fn parse_boolean(parser: &mut Parser) -> Option<Expression> {
    let val = match parser.current_token.token_type {
        TokenType::True => true,
        TokenType::False => false,
        _ => return None,
    };

    Some(Expression::Boolean(val))
}

fn parse_infix(parser: &mut Parser, left: Expression) -> Option<Expression> {
    let operator = parser.current_token.clone();
    let precedence = token_precedence(&operator.token_type);
    parser.next_token();
    let right = parse_expression(parser, precedence)?;

    Some(Expression::Infix(InfixExpression {
        left: Box::new(left),
        operator,
        right: Box::new(right),
    }))
}

fn parse_group(parser: &mut Parser) -> Option<Expression> {
    parser.next_token();

    let expression = parse_expression(parser, Precedence::Lowest)?;

    parser.peek_expected(TokenType::RParen)?;
    parser.next_token();

    Some(expression)
}

fn parse_if(parser: &mut Parser) -> Option<Expression> {
    parser.next_token();
    let condition = parse_expression(parser, Precedence::Lowest)?;

    parser.peek_expected(TokenType::LBrace)?;
    let consequence = parse_block(parser)?;
    let mut alternative = None;

    if parser.peek_token.token_type == TokenType::Else {
        parser.next_token();
        parser.peek_expected(TokenType::LBrace)?;
        alternative = Some(Box::new(parse_block(parser)?));
    }

    Some(Expression::If(IfExpression {
        condition: Box::new(condition),
        consequence: Box::new(consequence),
        alternative,
    }))
}

fn parse_while(parser: &mut Parser) -> Option<Expression> {
    parser.next_token();
    let condition = parse_expression(parser, Precedence::Lowest)?;

    parser.peek_expected(TokenType::LBrace)?;
    let body = parse_block(parser)?;

    Some(Expression::While(WhileExpression {
        condition: Box::new(condition),
        body: Box::new(body),
    }))
}

fn parse_block(parser: &mut Parser) -> Option<Statement> {
    parser.next_token();
    let mut statements = Vec::new();

    while parser.current_token.token_type != TokenType::RBrace
        && parser.current_token.token_type != TokenType::EOF
    {
        if let Some(statement) = parse_statement(parser) {
            statements.push(statement);
        }
        parser.next_token();
    }

    Some(Statement::Block(statements))
}

fn parse_fn_literal(parser: &mut Parser) -> Option<Expression> {
    parser.peek_expected(TokenType::LParen)?;
    parser.next_token();

    let parameters = parse_fn_parameters(parser)?;
    parser.peek_expected(TokenType::LBrace)?;
    let body = Box::new(parse_block(parser)?);

    Some(Expression::Function(FunctionExpression {
        parameters,
        body,
    }))
}

fn parse_fn_parameters(parser: &mut Parser) -> Option<Vec<Expression>> {
    let mut identifiers = Vec::new();

    if parser.peek_token.token_type == TokenType::RParen {
        parser.next_token();
        return Some(identifiers);
    }

    parser.next_token();
    let TokenType::Identifier(name) = &parser.current_token.token_type else {
        return None;
    };
    let identifier = Expression::Identifier(name.clone());
    identifiers.push(identifier);

    while parser.peek_token.token_type == TokenType::Comma {
        parser.next_token();
        parser.next_token();
        let TokenType::Identifier(name) = &parser.current_token.token_type else {
            return None;
        };
        let identifier = Expression::Identifier(name.clone());
        identifiers.push(identifier);
    }

    parser.peek_expected(TokenType::RParen)?;
    parser.next_token();

    Some(identifiers)
}

fn parse_call(parser: &mut Parser, function: Expression) -> Option<Expression> {
    let args = parse_call_args(parser)?;
    Some(Expression::Call(CallExpression {
        function: Box::new(function),
        args,
    }))
}

fn parse_call_args(parser: &mut Parser) -> Option<Vec<Expression>> {
    let mut args = Vec::new();

    if parser.peek_token.token_type == TokenType::RParen {
        parser.next_token();
        return Some(args);
    }

    parser.next_token();
    args.push(parse_expression(parser, Precedence::Lowest)?);

    while parser.peek_token.token_type == TokenType::Comma {
        parser.next_token();
        parser.next_token();
        args.push(parse_expression(parser, Precedence::Lowest)?);
    }

    parser.peek_expected(TokenType::RParen)?;
    parser.next_token();

    Some(args)
}

fn parse_array(parser: &mut Parser) -> Option<Expression> {
    let elements = parse_expression_list(parser, TokenType::RBracket)?;

    Some(Expression::Array(elements))
}

fn parse_expression_list(parser: &mut Parser, end: TokenType) -> Option<Vec<Expression>> {
    let mut list = Vec::new();

    if parser.peek_token.token_type == end {
        parser.next_token();
        return Some(list);
    }

    parser.next_token();
    list.push(parse_expression(parser, Precedence::Lowest)?);

    while matches!(parser.peek_token.token_type, TokenType::Comma) {
        parser.next_token();
        parser.next_token();
        list.push(parse_expression(parser, Precedence::Lowest)?);
    }

    parser.peek_expected(end)?;
    parser.next_token();

    Some(list)
}

fn parse_index(parser: &mut Parser, left: Expression) -> Option<Expression> {
    parser.next_token();
    let index = parse_expression(parser, Precedence::Lowest)?;

    parser.peek_expected(TokenType::RBracket)?;
    parser.next_token();

    Some(Expression::Index(IndexExpression {
        left: Box::new(left),
        index: Box::new(index),
    }))
}

fn token_precedence(token: &TokenType) -> Precedence {
    match token {
        TokenType::Assign => Precedence::Assign,
        TokenType::Equal | TokenType::NotEqual => Precedence::Equals,
        TokenType::LessThan | TokenType::GreaterThan => Precedence::LessGreater,
        TokenType::Plus | TokenType::Minus => Precedence::Sum,
        TokenType::Slash | TokenType::Asterisk => Precedence::Product,
        TokenType::LParen => Precedence::Call,
        TokenType::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::IfExpression;

    use super::*;

    #[test]
    fn var_statements() {
        let input = "
            let foo = 5;
            let bar = 100;
        ";
        let expected_identifiers = vec![
            TokenType::Identifier("foo".to_owned()),
            TokenType::Identifier("bar".to_owned()),
        ];
        let statements = setup_test(&input, 2);

        for (i, statement) in statements.iter().enumerate() {
            match statement {
                Statement::VariableAssign(LetStatement {
                    identifier,
                    expression: _,
                }) => {
                    assert_eq!(identifier.token_type, expected_identifiers[i]);
                }
                _ => panic!("Expected to parse only var statements"),
            }
        }
    }

    #[test]
    fn return_statements() {
        let input = "
            return 5;
            return 100_000;
            return 8.23;
        ";
        let statements = setup_test(&input, 3);

        for statement in statements.iter() {
            match statement {
                Statement::Return(_) => {}
                _ => panic!("Expected to parse only return statements"),
            }
        }
    }

    #[test]
    fn expression_identifier() {
        let input = "
            foobar;
            hello_world;
        ";
        let expected_expressions = vec![
            Expression::Identifier("foobar".to_owned()),
            Expression::Identifier("hello_world".to_owned()),
        ];
        let statements = setup_test(&input, 2);
        check_expressions_match(statements, expected_expressions);
    }

    #[test]
    fn expression_numbers() {
        let input = "
            5;
            1.23;
        ";
        let expected_expressions = vec![Expression::Int(5), Expression::Float(1.23)];
        let statements = setup_test(&input, 2);
        check_expressions_match(statements, expected_expressions);
    }

    #[test]
    fn expression_strings() {
        let input = "
            \"Hello World\";
            \"Goodbye\";
        ";
        let expected_expressions = vec![
            Expression::Str("Hello World".to_owned()),
            Expression::Str("Goodbye".to_owned()),
        ];
        let statements = setup_test(&input, 2);
        check_expressions_match(statements, expected_expressions);
    }

    #[test]
    fn expression_prefix() {
        let input = "
            !5;
            -1.23;
            !true;
        ";
        let expected_expressions = vec![
            Expression::Prefix(PrefixExpression {
                operator: Token {
                    token_type: TokenType::Bang,
                },
                right: Box::new(Expression::Int(5)),
            }),
            Expression::Prefix(PrefixExpression {
                operator: Token {
                    token_type: TokenType::Minus,
                },
                right: Box::new(Expression::Float(1.23)),
            }),
            Expression::Prefix(PrefixExpression {
                operator: Token {
                    token_type: TokenType::Bang,
                },
                right: Box::new(Expression::Boolean(true)),
            }),
        ];
        let statements = setup_test(&input, 3);
        check_expressions_match(statements, expected_expressions);
    }

    #[test]
    fn expression_infix() {
        let input = "
            5 + 10;
            20 - 1.23;
            33 * 400;
            3 / 2;
            12 > 32;
            44 < 38;
            33 == 41;
            87 != 828;
            true == false;
        ";

        let expected_expressions = vec![
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Int(5)),
                operator: Token {
                    token_type: TokenType::Plus,
                },
                right: Box::new(Expression::Int(10)),
            }),
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Int(20)),
                operator: Token {
                    token_type: TokenType::Minus,
                },
                right: Box::new(Expression::Float(1.23)),
            }),
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Int(33)),
                operator: Token {
                    token_type: TokenType::Asterisk,
                },
                right: Box::new(Expression::Int(400)),
            }),
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Int(3)),
                operator: Token {
                    token_type: TokenType::Slash,
                },
                right: Box::new(Expression::Int(2)),
            }),
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Int(12)),
                operator: Token {
                    token_type: TokenType::GreaterThan,
                },
                right: Box::new(Expression::Int(32)),
            }),
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Int(44)),
                operator: Token {
                    token_type: TokenType::LessThan,
                },
                right: Box::new(Expression::Int(38)),
            }),
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Int(33)),
                operator: Token {
                    token_type: TokenType::Equal,
                },
                right: Box::new(Expression::Int(41)),
            }),
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Int(87)),
                operator: Token {
                    token_type: TokenType::NotEqual,
                },
                right: Box::new(Expression::Int(828)),
            }),
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Boolean(true)),
                operator: Token {
                    token_type: TokenType::Equal,
                },
                right: Box::new(Expression::Boolean(false)),
            }),
        ];
        let statements = setup_test(&input, 9);
        check_expressions_match(statements, expected_expressions);
    }

    #[test]
    fn check_precendence() {
        let input = "
            3 - 2 * 1;
            (3 - 2) * 1;
            3 - (2 - 1);
            -(3 - 2);
        ";

        let expected_expressions = vec![
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Int(3)),
                operator: Token {
                    token_type: TokenType::Minus,
                },
                right: Box::new(Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Int(2)),
                    operator: Token {
                        token_type: TokenType::Asterisk,
                    },
                    right: Box::new(Expression::Int(1)),
                })),
            }),
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Int(3)),
                    operator: Token {
                        token_type: TokenType::Minus,
                    },
                    right: Box::new(Expression::Int(2)),
                })),
                operator: Token {
                    token_type: TokenType::Asterisk,
                },
                right: Box::new(Expression::Int(1)),
            }),
            Expression::Infix(InfixExpression {
                left: Box::new(Expression::Int(3)),
                operator: Token {
                    token_type: TokenType::Minus,
                },
                right: Box::new(Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Int(2)),
                    operator: Token {
                        token_type: TokenType::Minus,
                    },
                    right: Box::new(Expression::Int(1)),
                })),
            }),
            Expression::Prefix(PrefixExpression {
                operator: Token {
                    token_type: TokenType::Minus,
                },
                right: Box::new(Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Int(3)),
                    operator: Token {
                        token_type: TokenType::Minus,
                    },
                    right: Box::new(Expression::Int(2)),
                })),
            }),
        ];

        let statements = setup_test(&input, 4);
        check_expressions_match(statements, expected_expressions);
    }

    #[test]
    fn expression_boolean() {
        let input = "
            true;
            false;
            let foo = true;
            let bar = false;
        ";

        let expected_statements = vec![
            Statement::Expression(Expression::Boolean(true)),
            Statement::Expression(Expression::Boolean(false)),
            Statement::VariableAssign(LetStatement {
                identifier: Token {
                    token_type: TokenType::Identifier("foo".to_owned()),
                },
                expression: Expression::Boolean(true),
            }),
            Statement::VariableAssign(LetStatement {
                identifier: Token {
                    token_type: TokenType::Identifier("bar".to_owned()),
                },
                expression: Expression::Boolean(false),
            }),
        ];
        let statements = setup_test(&input, 4);

        for (i, statement) in statements.iter().enumerate() {
            assert_eq!(*statement, expected_statements[i]);
        }
    }

    #[test]
    fn expression_if() {
        let input = "
            if x < y { x };
            if x > y { x } else { y };
        ";

        let expected_expressions = vec![
            Expression::If(IfExpression {
                condition: Box::new(Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Identifier("x".to_owned())),
                    operator: Token {
                        token_type: TokenType::LessThan,
                    },
                    right: Box::new(Expression::Identifier("y".to_owned())),
                })),
                consequence: Box::new(Statement::Block(vec![Statement::Expression(
                    Expression::Identifier("x".to_owned()),
                )])),
                alternative: None,
            }),
            Expression::If(IfExpression {
                condition: Box::new(Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Identifier("x".to_owned())),
                    operator: Token {
                        token_type: TokenType::GreaterThan,
                    },
                    right: Box::new(Expression::Identifier("y".to_owned())),
                })),
                consequence: Box::new(Statement::Block(vec![Statement::Expression(
                    Expression::Identifier("x".to_owned()),
                )])),
                alternative: Some(Box::new(Statement::Block(vec![Statement::Expression(
                    Expression::Identifier("y".to_owned()),
                )]))),
            }),
        ];

        let statements = setup_test(&input, 2);
        check_expressions_match(statements, expected_expressions);
    }

    #[test]
    fn expression_function() {
        let input = "
            fn(x, y) { x + y; }
        ";

        let expected_expressions = vec![Expression::Function(FunctionExpression {
            parameters: vec![
                Expression::Identifier("x".to_owned()),
                Expression::Identifier("y".to_owned()),
            ],
            body: Box::new(Statement::Block(vec![Statement::Expression(
                Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Identifier("x".to_owned())),
                    operator: Token {
                        token_type: TokenType::Plus,
                    },
                    right: Box::new(Expression::Identifier("y".to_owned())),
                }),
            )])),
        })];

        let statements = setup_test(&input, 1);
        check_expressions_match(statements, expected_expressions);
    }

    #[test]
    fn expression_call() {
        let input = "
            foo(1, 2 * 3, 4 + 5);
        ";

        let expected_expressions = vec![Expression::Call(CallExpression {
            function: Box::new(Expression::Identifier("foo".to_owned())),
            args: vec![
                Expression::Int(1),
                Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Int(2)),
                    operator: Token {
                        token_type: TokenType::Asterisk,
                    },
                    right: Box::new(Expression::Int(3)),
                }),
                Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Int(4)),
                    operator: Token {
                        token_type: TokenType::Plus,
                    },
                    right: Box::new(Expression::Int(5)),
                }),
            ],
        })];

        let statements = setup_test(&input, 1);
        check_expressions_match(statements, expected_expressions);
    }

    #[test]
    fn arrays() {
        let input = "
            [1, 2 * 2, 5]
            foo[2]
        ";

        let expected_expressions = vec![
            Expression::Array(vec![
                Expression::Int(1),
                Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Int(2)),
                    operator: Token {
                        token_type: TokenType::Asterisk,
                    },
                    right: Box::new(Expression::Int(2)),
                }),
                Expression::Int(5),
            ]),
            Expression::Index(IndexExpression {
                left: Box::new(Expression::Identifier("foo".to_owned())),
                index: Box::new(Expression::Int(2)),
            }),
        ];

        let statements = setup_test(&input, 2);
        check_expressions_match(statements, expected_expressions);
    }

    #[test]
    fn while_loops() {
        let input = "
            while true {
                5
            }
        ";
        let expected_expressions = vec![Expression::While(WhileExpression {
            condition: Box::new(Expression::Boolean(true)),
            body: Box::new(Statement::Block(vec![Statement::Expression(
                Expression::Int(5),
            )])),
        })];

        let statements = setup_test(&input, 1);
        check_expressions_match(statements, expected_expressions);
    }

    #[test]
    fn variable_assign() {
        let input = "
            foo = 3
        ";
        let expected_expressions = vec![Expression::Infix(InfixExpression {
            left: Box::new(Expression::Identifier("foo".to_owned())),
            operator: Token {
                token_type: TokenType::Assign,
            },
            right: Box::new(Expression::Int(3)),
        })];

        let statements = setup_test(&input, 1);
        check_expressions_match(statements, expected_expressions);
    }

    fn setup_test(input: &str, expected_statement_count: usize) -> Vec<Statement> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parsing_errors(&parser);

        let Statement::Program(statements) = program else {
            panic!("parse_program needs to return a Statement::Program");
        };
        assert_eq!(statements.len(), expected_statement_count);

        statements
    }

    fn check_expressions_match(statements: Vec<Statement>, expected_expressions: Vec<Expression>) {
        for (i, statement) in statements.iter().enumerate() {
            let Statement::Expression(expression) = statement else {
                panic!("Expected to parse only expression statements");
            };
            assert_eq!(*expression, expected_expressions[i]);
        }
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
