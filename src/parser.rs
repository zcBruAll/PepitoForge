/* PepitoCode Grammar
 * --------------------------
 * program              ::= statement*
 * statement            ::= var_declaration | expression_statement
 * var_declaration      ::= 'var' identifier '=' expression ';'
 * expression_statement ::= expression ';'
 * expression           ::= term (('+' | '-') term)*
 * term                 ::= factor (('*' | '/') factor)*
 * factor               ::= number | identifier | '(' expression ')'
 * ==========================================================
 * This allows:
 * var x = 10;
 * var y = x + 10;
 * x + 5 * y;
*/

use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(i32),
    Variable(String),
    BinaryOp(Box<Expr>, Op, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDeclaration(String, Expr),
    Expression(Expr),
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Add,
    Substract,
    Multiply,
    Divide,
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

    pub fn parse_statement(&mut self) -> Option<Stmt> {
        match self.current_token() {
            Some(Token::Var) => self.parse_var_declaration(),
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
    use crate::parser::Op;
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