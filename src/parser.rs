use crate::{ast::{ASTNode, Expression, Operator}, lexer::Token};

/// Parser struct holds the tokens and the current position in the token stream
pub struct Parser {
    tokens: Vec<Token>, // List of tokens produced by the lexer
    position: usize,    // Current position in the token list
}

impl Parser {
    /// Constructor to create a new Parser
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    /// Function to parse tokens into an AST
    pub fn parse(&mut self) -> Vec<ASTNode> {
        let mut ast: Vec<ASTNode> = Vec::new();

        let mut text: Vec<Box<Expression>> = Vec::new();
        let mut variables: Vec<(String, String, Box<Expression>)> = Vec::new();
        let mut _return: Box<Expression> = Box::new(Expression::Integer(0));
        while let Some(token) = self.tokens.get(self.position) {
            match token {
                Token::Return => {
                    self.position += 1;
                    let expr = self.expect_operand();
                    self.expect_token(Token::Semicolon); // Expect a semicolon
                    _return = Box::new(expr);
                }

                Token::Int => {
                    self.position += 1;
                    let var_name = match self
                        .get_next_token()
                        .expect("Expected an identifier after 'Int'")
                    {
                        Token::Identifier(name) => name,
                        _ => panic!("Expected an identifier after 'Int'"),
                    };
                    self.expect_token(Token::Assign);
                    let value = match self
                        .get_next_token()
                        .expect("Expected an expression after assignment")
                    {
                        Token::Number(n) => Expression::Integer(n as i32),
                        Token::Identifier(name) => Expression::Identifier(name),
                        _ => panic!("Expected a number or identifier after assignment"),
                    };
                    self.expect_token(Token::Semicolon);
                    variables.push((var_name, "dq".to_string(), Box::new(value)));
                }

                Token::String => {
                    self.position += 1;
                    let var_name = match self
                        .get_next_token()
                        .expect("Expected an identifier after 'String'")
                    {
                        Token::Identifier(name) => name,
                        _ => panic!("Expected an identifier after 'String'")
                    };
                    self.expect_token(Token::Assign);
                    let value = match self
                        .get_next_token()
                        .expect("Expected an expression after assignment")
                    {
                        Token::StringLiteral(s) => Expression::String(s),
                        Token::Identifier(name) => Expression::Identifier(name),
                        _ => panic!("Expected a string or identifier after assignment")
                    };
                    self.expect_token(Token::Semicolon);
                    variables.push((var_name, "db".to_string(), Box::new(value)))
                }

                Token::Print => {
                    self.position += 1;
                    self.expect_token(Token::BracketO);
                    let expr = self.expect_expression();
                    self.expect_token(Token::BracketC);
                    self.expect_token(Token::Semicolon);
                    text.push(Box::new(Expression::Print(Box::new(expr))));
                }

                _ => panic!("Unexpected token"),
            }
        }

        ast.push(ASTNode::VariableDeclaration(variables));
        ast.push(ASTNode::Content(text));
        ast.push(ASTNode::Return(_return));
        ast
    }

    /// Helper function to expect a specific token and panic if not found
    fn expect_token(&mut self, expected: Token) {
        let token = self.get_next_token();
        if token != Some(expected.clone()) {
            panic!("Expected {:?}, got {:?}", expected, token);
        }
    }

    /// Helper function to get the next token
    fn get_next_token(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.position).cloned();
        self.position += 1;
        token
    }

    pub fn expect_expression(&mut self) -> Expression {
        let left = self.expect_operand();
        let token = self.get_next_token().expect("Expected an expression");
        match token {
            Token::Plus => {
                let right = self.expect_operand();
                Expression::BinaryOperation(Box::new(left), Operator::Add, Box::new(right))
            }
            Token::Minus => {
                let right = self.expect_operand();
                Expression::BinaryOperation(Box::new(left), Operator::Subtract, Box::new(right))
            }
            Token::Multiply => {
                let right = self.expect_operand();
                Expression::BinaryOperation(Box::new(left), Operator::Multiply, Box::new(right))
            }
            Token::Divide => {
                let right = self.expect_operand();
                Expression::BinaryOperation(Box::new(left), Operator::Divide, Box::new(right))
            }
            // Handle other operators similarly
            _ => left, // If there's no operator, return the operand as is
        }
    }

    fn expect_operand(&mut self) -> Expression {
        let token = self.get_next_token().expect("Expected an operand");
        match token {
            Token::Number(n) => {
                if n.fract() == 0.0 {
                    Expression::Integer(n as i32)
                } else {
                    Expression::Float(n)
                }
            }
            Token::Identifier(name) => Expression::Identifier(name),
            _ => panic!("Expected a number or identifier"),
        }
    }
}