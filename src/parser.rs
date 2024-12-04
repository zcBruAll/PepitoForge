/* PepitoCode Grammar
 * --------------------------
 * program                  ::= statement*
 * statement                ::= var_declaration | expression_statement | conditional_statement
 * var_declaration          ::= 'var' identifier '=' expression ';'
 * expression_statement     ::= expression ';'
 * conditional_statement    ::= 'if' '(' condition ')' block ('else if' '(' expression ')' block)* ('else' block)?
 * condition                ::= logical_and ('||' logical_and)*
 * logical_and              ::= comparison ('&&' comparison)*
 * comparison               ::= expression ('<' | '>' | '<=' | '>=' | '==' | '!=') expression
 * block                    ::= '{' statement* '}'
 * expression               ::= term (('+' | '-') term)*
 * term                     ::= factor (('*' | '/') factor)*
 * factor                   ::= number | identifier | '(' expression ')'
 * ==========================================================
 * This allows:
 * if (x > 10) {
 *     var y = x + 5;
 * } else if (x == 10) {
 *     var y = x * 2;
 * } else {
 *     var y = 0;
 * }
 */

use core::panic;

use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(i32),
    Variable(String),
    BinaryOp(Box<Expr>, Op, Box<Expr>),
    BooleanOp(Box<Expr>, BoolOp, Box<Expr>),
    LogicalOp(Box<Expr>, LogicalOp, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDeclaration(String, Expr),
    Expression(Expr),
    If {
        condition: Box<Expr>,
        then_block: Vec<Stmt>,
        else_block: Option<Vec<Stmt>>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Add,
    Substract,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq)]
pub enum BoolOp {
    Greater,
    Lower,
    GreaterOrEq,
    LowerOrEq,
    Equal,
    NotEqual,
}

#[derive(Debug, PartialEq)]
pub enum LogicalOp {
    And,
    Or,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    // program ::= statement*
    pub fn parse_program(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();

        while self.current_token().is_some() && self.current_token() != Some(&Token::EOF) {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            } else {
                eprintln!("Syntax error at token {:?}", self.current_token());
                self.advance();
            }
        }

        statements
    }

    // statement ::= var_declaration | expression_statement | conditional_statement
    pub fn parse_statement(&mut self) -> Option<Stmt> {
        match self.current_token() {
            Some(Token::Var) => self.parse_var_declaration(),
            Some(Token::If) => self.parse_conditional_statement(),
            _ => self.parse_expression_statement()
        }
    }

    // var_declaration ::= 'var' identifier '=' expression ';'
    pub fn parse_var_declaration(&mut self) -> Option<Stmt> {
        if let Some(Token::Var) = self.current_token() {
            self.advance();

            if let Some(Token::Identifier(name)) = self.current_token() {
                let var_name = name.clone();
                self.advance();

                if let Some(Token::Equals) = self.current_token() {
                    self.advance();

                    let expr = self.parse_expression()?;
                    if let Some(Token::Semicolon) = self.current_token() {
                        self.advance();
                        return Some(Stmt::VarDeclaration(var_name, expr));
                    }
                }
            }
        }

        None
    }

    // expression_statement ::= expression ';'
    pub fn parse_expression_statement(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression()?;
        if let Some(Token::Semicolon) = self.current_token() {
            self.advance();
            return Some(Stmt::Expression(expr));
        }
        None
    }

    // conditional_statement ::= 'if' '(' condition ')' block ('else if' '(' expression ')' block)* ('else' block)?
    pub fn parse_conditional_statement(&mut self) -> Option<Stmt> {
        if let Some(Token::If) = self.current_token() {
            self.advance();
            if let Some(Token::LParen) = self.current_token() {
                self.advance();
                let condition = self.parse_condition()?;
                if let Some(Token::RParen) = self.current_token() {
                    self.advance();
                } else {
                    panic!("Expected ')' after the if condition");
                }

                let then_block = self.parse_block()?;

                let else_block = self.parse_else_block();

                return Some(Stmt::If { 
                    condition: Box::new(condition), 
                    then_block, 
                    else_block, })
            } else {
                panic!("Expected '(' after 'if'");
            }
        }
        
        None
    }

    // condition ::= logical_and ('||' logical_and)*
    pub fn parse_condition(&mut self) -> Option<Expr> {
        let mut cond = self.parse_logical_and()?;

        if let Some(Token::Pipe) = self.current_token() {
            self.advance();
            if let Some(Token::Pipe) = self.current_token() {
                self.advance();
                let right = self.parse_logical_and()?;
                cond = Expr::LogicalOp(Box::new(cond), LogicalOp::Or, Box::new(right));
            } else {
                panic!("Unexpected token after '|'");
            }
        }

        Some(cond)
    }

    // logical_and ::= comparison ('&&' comparison)*
    pub fn parse_logical_and(&mut self) -> Option<Expr> {
        let mut comp = self.parse_comparison()?;

        if let Some(Token::Ampersand) = self.current_token() {
            self.advance();
            if let Some(Token::Ampersand) = self.current_token() {
                self.advance();
                let right = self.parse_comparison()?;
                comp = Expr::LogicalOp(Box::new(comp), LogicalOp::And, Box::new(right));
            } else {
                panic!("Unecpected token after '&'");
            }
        }

        Some(comp)
    }

    // comparison ::= expression ('<' | '>' | '<=' | '>=' | '==' | '!=') expression
    pub fn parse_comparison(&mut self) -> Option<Expr> {
        let mut expr = self.parse_expression()?;

        match self.current_token()? {
            Token::Lower => {
                self.advance();
                let mut op = BoolOp::Lower;
                if let Some(Token::Equals) = self.current_token() {
                    self.advance();
                    op = BoolOp::LowerOrEq;
                }
                let right = self.parse_expression()?;
                expr = Expr::BooleanOp(Box::new(expr), op, Box::new(right));
            }
            Token::Greater => {
                self.advance();
                let mut op = BoolOp::Greater;
                if let Some(Token::Equals) = self.current_token() {
                    self.advance();
                    op = BoolOp::GreaterOrEq
                }
                let right = self.parse_expression()?;
                expr = Expr::BooleanOp(Box::new(expr), op, Box::new(right));
            }
            Token::Equals => {
                self.advance();
                if let Some(Token::Equals) = self.current_token() {
                    self.advance();
                    let right = self.parse_expression()?;
                    expr = Expr::BooleanOp(Box::new(expr), BoolOp::Equal, Box::new(right));
                }
            }
            Token::Exclamation => {
                self.advance();
                if let Some(Token::Equals) = self.current_token() {
                    self.advance();
                    let right = self.parse_expression()?;
                    expr = Expr::BooleanOp(Box::new(expr), BoolOp::NotEqual, Box::new(right));
                }
            }
            _ => panic!("Invalid condition"),
        }

        Some(expr)
    }

    // else_block ::= ('else' block)?
    pub fn parse_else_block(&mut self) -> Option<Vec<Stmt>> {
        if let Some(Token::Else) = self.current_token() {
            self.advance();

            if let Some(Token::If) = self.current_token() {
                return Some(vec![self.parse_conditional_statement()?])
            }

            return self.parse_block();
        }

        None
    }

    // block ::= '{' statement* '}'
    pub fn parse_block(&mut self) -> Option<Vec<Stmt>> {
        if let Some(Token::LBrace) = self.current_token() {
            self.advance();

            let mut stmts = Vec::new();
            while let Some(token) = self.current_token() {
                if token == &Token::RBrace {
                    break;
                }
                let stmt = self.parse_statement()?;
                stmts.push(stmt);
            }

            if let Some(Token::RBrace) = self.current_token() {
                self.advance();
                return Some(stmts);
            } else {
                panic!("Expected '}}' to close block");
            }
        }

        None
    }

    // expression ::= term (('+' | '-') term)*
    pub fn parse_expression(&mut self) -> Option<Expr> {
        let mut expr = self.parse_term()?;

        while let Some(token) = self.current_token() {
            match token {
                Token::Plus => {
                    self.advance();
                    let right = self.parse_term()?;
                    expr = Expr::BinaryOp(Box::new(expr), Op::Add, Box::new(right));
                }
                Token::Minus => {
                    self.advance();
                    let right = self.parse_term()?;
                    expr = Expr::BinaryOp(Box::new(expr), Op::Substract, Box::new(right));
                }
                _ => break,
            }
        }

        Some(expr)
    }

    // term ::= factor (('*' | '/') factor)*
    pub fn parse_term(&mut self) -> Option<Expr> {
        let mut expr = self.parse_factor()?;
        
        while let Some(token) = self.current_token() {
            match token {
                Token::Star => {
                    self.advance();
                    let right = self.parse_factor()?;
                    expr = Expr::BinaryOp(Box::new(expr), Op::Multiply, Box::new(right));
                }
                Token::Slash => {
                    self.advance();
                    let right = self.parse_factor()?;
                    expr = Expr::BinaryOp(Box::new(expr), Op::Divide, Box::new(right));
                }
                _ => break,
            }
        }

        Some(expr)
    }

    // factor ::= number | identifier | '(' expression ')'
    fn parse_factor(&mut self) -> Option<Expr> {
        match self.current_token().cloned() {
            Some(Token::Number(value)) => {
                self.advance();
                Some(Expr::Literal(value))
            }
            Some(Token::Identifier(name)) => {
                self.advance();
                Some(Expr::Variable(name))
            }
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                if let Some(Token::RParen) = self.current_token() {
                    self.advance();
                    Some(expr)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{BoolOp, LogicalOp, Op};
    use crate::{lexer::Lexer, parser::{Expr, Parser, Stmt}};

    #[test]
    fn test_var_declaration() {
    let input = "var x = 10;";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.tokenize(); // Assuming you have a tokenize method
    let mut parser = Parser::new(tokens);

    let stmt = parser.parse_var_declaration();
    assert_eq!(stmt, Some(Stmt::VarDeclaration("x".to_string(), Expr::Literal(10))));
    }

    #[test]
    fn test_expression() {
        let input = "10 + 20 * 2;";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let expr = parser.parse_expression();
        assert_eq!(
            expr,
            Some(Expr::BinaryOp(
                Box::new(Expr::Literal(10)),
                Op::Add,
                Box::new(Expr::BinaryOp(
                    Box::new(Expr::Literal(20)),
                    Op::Multiply,
                    Box::new(Expr::Literal(2))
                ))
            ))
        );
    }

    #[test]
    fn test_if_elseif_else() {
        let input = "
            if (x > 10) {
                var y = x * 2;
            } else if (x > 5) {
                var y = x + 1;
            } else {
                var y = 0;
            }
        ";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let cond = parser.parse_conditional_statement();
        assert_eq!(
            cond,
            Some(Stmt::If {
                condition: Box::new(Expr::BooleanOp(
                    Box::new(Expr::Variable("x".to_string())),
                    BoolOp::Greater,
                    Box::new(Expr::Literal(10)),
                )),
                then_block: vec![
                    Stmt::VarDeclaration(
                        "y".to_string(),
                        Expr::BinaryOp(
                            Box::new(Expr::Variable("x".to_string())),
                            Op::Multiply,
                            Box::new(Expr::Literal(2)),
                        ),
                    ),
                ],
                else_block: Some(vec![
                    Stmt::If {
                        condition: Box::new(Expr::BooleanOp(
                            Box::new(Expr::Variable("x".to_string())),
                            BoolOp::Greater,
                            Box::new(Expr::Literal(5)),
                        )),
                        then_block: vec![
                            Stmt::VarDeclaration(
                                "y".to_string(),
                                Expr::BinaryOp(
                                    Box::new(Expr::Variable("x".to_string())),
                                    Op::Add,
                                    Box::new(Expr::Literal(1)),
                                ),
                            ),
                        ],
                        else_block: Some(vec![
                            Stmt::VarDeclaration(
                                "y".to_string(),
                                Expr::Literal(0),
                            ),
                        ]),
                    },
                ]),
            }
            )
        )
    }

    #[test]
    fn test_if_else() {
        let input = "
            if (x > 0) {
                var a = 1;
            } else {
                if (y == 2) {
                    var b = 2;
                }
            }
        ";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let cond = parser.parse_conditional_statement();
        assert_eq!(cond,
            Some(Stmt::If {
                condition: Box::new(Expr::BooleanOp(
                    Box::new(Expr::Variable("x".to_string())),
                    BoolOp::Greater,
                    Box::new(Expr::Literal(0)),
                )),
                then_block: vec![
                    Stmt::VarDeclaration("a".to_string(), Expr::Literal(1)),
                ],
                else_block: Some(vec![
                    Stmt::If {
                        condition: Box::new(Expr::BooleanOp(
                            Box::new(Expr::Variable("y".to_string())),
                            BoolOp::Equal,
                            Box::new(Expr::Literal(2)),
                        )),
                        then_block: vec![
                            Stmt::VarDeclaration("b".to_string(), Expr::Literal(2)),
                        ],
                        else_block: None,
                    },
                ]),
            })
        );
    }

    #[test]
    fn test_if() {
        let input = "if (x > 10) { var y = x + 5; }";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
    
        let cond = parser.parse_conditional_statement();
        assert_eq!(
            cond,
            Some(Stmt::If {
                condition: Box::new(Expr::BooleanOp(
                    Box::new(Expr::Variable("x".to_string())),
                    BoolOp::Greater,
                    Box::new(Expr::Literal(10)),
                )),
                then_block: vec![
                    Stmt::VarDeclaration(
                        "y".to_string(),
                        Expr::BinaryOp(
                            Box::new(Expr::Variable("x".to_string())),
                            Op::Add,
                            Box::new(Expr::Literal(5)),
                        ),
                    ),
                ],
                else_block: None,
            })
        );
    }

    #[test]
    fn test_if_else_if() {
        let input = "
            if (x > 10) {
                var y = x + 5;
            } else if (x == 5) {
                var y = x - 2;
            }
        ";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let cond = parser.parse_conditional_statement();
        assert_eq!(
            cond,
            Some(Stmt::If {
                condition: Box::new(Expr::BooleanOp(
                    Box::new(Expr::Variable("x".to_string())),
                    BoolOp::Greater,
                    Box::new(Expr::Literal(10)),
                )),
                then_block: vec![
                    Stmt::VarDeclaration(
                        "y".to_string(),
                        Expr::BinaryOp(
                            Box::new(Expr::Variable("x".to_string())),
                            Op::Add,
                            Box::new(Expr::Literal(5)),
                        ),
                    ),
                ],
                else_block: Some(vec![
                    Stmt::If {
                        condition: Box::new(Expr::BooleanOp(
                            Box::new(Expr::Variable("x".to_string())),
                            BoolOp::Equal,
                            Box::new(Expr::Literal(5)),
                        )),
                        then_block: vec![
                            Stmt::VarDeclaration(
                                "y".to_string(),
                                Expr::BinaryOp(
                                    Box::new(Expr::Variable("x".to_string())),
                                    Op::Substract,
                                    Box::new(Expr::Literal(2)),
                                ),
                            ),
                        ],
                        else_block: None,
                    },
                ]),
            })
        );
    }

    #[test]
    fn test_if_empty_block() {
        let input = "if (x < 5) {}";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let cond = parser.parse_conditional_statement();
        assert_eq!(
            cond,
            Some(Stmt::If {
                condition: Box::new(Expr::BooleanOp(
                    Box::new(Expr::Variable("x".to_string())),
                    BoolOp::Lower,
                    Box::new(Expr::Literal(5)),
                )),
                then_block: vec![],
                else_block: None,
            })
        );
    }

    #[test]
    fn test_if_else_if_multiple() {
        let input = "
            if (x > 10) {
                var y = x + 5;
            } else if (x == 5) {
                var y = x - 2;
            } else if (x < 0) {
                var y = 0;
            }
        ";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let cond = parser.parse_conditional_statement();
        assert_eq!(
            cond,
            Some(Stmt::If {
                condition: Box::new(Expr::BooleanOp(
                    Box::new(Expr::Variable("x".to_string())),
                    BoolOp::Greater,
                    Box::new(Expr::Literal(10)),
                )),
                then_block: vec![
                    Stmt::VarDeclaration(
                        "y".to_string(),
                        Expr::BinaryOp(
                            Box::new(Expr::Variable("x".to_string())),
                            Op::Add,
                            Box::new(Expr::Literal(5)),
                        ),
                    ),
                ],
                else_block: Some(vec![
                    Stmt::If {
                        condition: Box::new(Expr::BooleanOp(
                            Box::new(Expr::Variable("x".to_string())),
                            BoolOp::Equal,
                            Box::new(Expr::Literal(5)),
                        )),
                        then_block: vec![
                            Stmt::VarDeclaration(
                                "y".to_string(),
                                Expr::BinaryOp(
                                    Box::new(Expr::Variable("x".to_string())),
                                    Op::Substract,
                                    Box::new(Expr::Literal(2)),
                                ),
                            ),
                        ],
                        else_block: Some(vec![
                            Stmt::If {
                                condition: Box::new(Expr::BooleanOp(
                                    Box::new(Expr::Variable("x".to_string())),
                                    BoolOp::Lower,
                                    Box::new(Expr::Literal(0)),
                                )),
                                then_block: vec![
                                    Stmt::VarDeclaration(
                                        "y".to_string(),
                                        Expr::Literal(0),
                                    ),
                                ],
                                else_block: None,
                            },
                        ]),
                    },
                ]),
            })
        );
    }

    #[test]
    fn test_condition_or() {
        let input = "
            x <= 5 || y >= 10
        ";

        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let condition = parser.parse_condition();

        assert_eq!(condition, 
            Some(            
                Expr::LogicalOp(
                Box::new(
                    Expr::BooleanOp(
                        Box::new(Expr::Variable("x".to_string())), 
                        BoolOp::LowerOrEq, 
                        Box::new(Expr::Literal(5))
                    )
                ), 
                LogicalOp::Or, 
                Box::new(
                    Expr::BooleanOp(
                        Box::new(
                            Expr::Variable("y".to_string())
                        ), 
                        BoolOp::GreaterOrEq, 
                        Box::new(
                            Expr::Literal(10)
                        )
                    )
                )
            ))
        )
    }

    #[test]
    fn test_condition_and() {
        let input = "
            x <= 5 && y >= 10
        ";

        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let condition = parser.parse_condition();

        assert_eq!(condition, 
            Some(            
                Expr::LogicalOp(
                Box::new(
                    Expr::BooleanOp(
                        Box::new(Expr::Variable("x".to_string())), 
                        BoolOp::LowerOrEq, 
                        Box::new(Expr::Literal(5))
                    )
                ), 
                LogicalOp::And, 
                Box::new(
                    Expr::BooleanOp(
                        Box::new(
                            Expr::Variable("y".to_string())
                        ), 
                        BoolOp::GreaterOrEq, 
                        Box::new(
                            Expr::Literal(10)
                        )
                    )
                )
            ))
        )
    }

    #[test]
    fn test_cond_or_and() {
        let input = "
            x <= 5 && y >= 10 || x == y
        ";

        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let condition = parser.parse_condition();

        assert_eq!(condition, 
            Some(
                Expr::LogicalOp(
                    Box::new(Expr::LogicalOp(
                        Box::new(
                            Expr::BooleanOp(
                                Box::new(Expr::Variable("x".to_string())), 
                                BoolOp::LowerOrEq, 
                                Box::new(Expr::Literal(5))
                            )
                        ), 
                        LogicalOp::And, 
                        Box::new(
                            Expr::BooleanOp(
                                Box::new(
                                    Expr::Variable("y".to_string())
                                ), 
                                BoolOp::GreaterOrEq, 
                                Box::new(
                                    Expr::Literal(10)
                                )
                            )
                        )
                    )), 
                    LogicalOp::Or,
                    Box::new(Expr::BooleanOp(
                        Box::new(Expr::Variable("x".to_string())), 
                        BoolOp::Equal,
                        Box::new(Expr::Variable("y".to_string()))
                    ))
                )
            )
        );
    }

    #[test]
    fn test_parse_program() {
        let input = "
            var x = 20;
            var y = x - 10;
            x + y * 2;
        ";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        let program = parser.parse_program();

        assert_eq!(
            program,
            vec![
                Stmt::VarDeclaration("x".to_string(), Expr::Literal(20)),
                Stmt::VarDeclaration("y".to_string(), Expr::BinaryOp(Box::new(Expr::Variable("x".to_string())), Op::Substract, Box::new(Expr::Literal(10)))),
                Stmt::Expression(
                    Expr::BinaryOp(
                        Box::new(Expr::Variable("x".to_string())),
                        Op::Add,
                        Box::new(Expr::BinaryOp(
                            Box::new(Expr::Variable("y".to_string())),
                            Op::Multiply,
                            Box::new(Expr::Literal(2))
                        ))
                    )
                )
            ]
        );
    }
}