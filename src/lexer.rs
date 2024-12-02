#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Var,                    // "var"
    Identifier(String),     // x
    Number(i32),            // 10
    Plus,                   // +
    Minus,                  // -
    Star,                   // *
    Slash,                  // /
    Equals,                 // =
    Semicolon,              // ;
    LParen,                 // (
    RParen,                 // )
    If,                     // "if"
    Else,                   // "else"
    LBrace,                 // {
    RBrace,                 // }
    Greater,                // >
    Lower,                  // <
    Exclamation,            // !
    EOF,                    // End Of File
}

pub struct Lexer {
    input: String,
    position: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer { input, position: 0 }
    }

    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.position)
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let ch = self.current_char();

        match ch {
            Some('+') => {
                self.advance();
                Token::Plus
            }
            Some('-') => {
                self.advance();
                Token::Minus
            }
            Some('*') => {
                self.advance();
                Token::Star
            }
            Some('/') => {
                self.advance();
                Token::Slash
            }
            Some('=') => {
                self.advance();
                Token::Equals
            }
            Some(';') => {
                self.advance();
                Token::Semicolon
            }
            Some('(') => {
                self.advance();
                Token::LParen
            }
            Some(')') => {
                self.advance();
                Token::RParen
            }
            Some('{') => {
                self.advance();
                Token::LBrace
            }
            Some('}') => {
                self.advance();
                Token::RBrace
            }
            Some('>') => {
                self.advance();
                Token::Greater
            }
            Some('<') => {
                self.advance();
                Token::Lower
            }
            Some('!') => {
                self.advance();
                Token::Exclamation
            }
            Some(c) if c.is_ascii_digit() => {
                let num = self.read_number();
                Token::Number(num)
            }
            Some(c) if c.is_ascii_alphabetic() => {
                let ident = self.read_identifier();
                match ident.as_str() {
                    "var" => Token::Var,
                    "if" => Token::If,
                    "else" => Token::Else,
                    _ => Token::Identifier(ident),
                }
            }
            None => Token::EOF,
            _ => panic!("Unexpected character"),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self) -> i32 {
        let mut number = String::new();

        while let Some(c) = self.current_char() {
            if c.is_ascii_digit() {
                number.push(c);
                self.advance();
            } else {
                break;
            }
        }
        number.parse().unwrap()
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();

        while let Some(c) = self.current_char() {
            if c.is_ascii_alphanumeric() {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }
        ident
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            if token == Token::EOF {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]

    fn test_lexer_variable() {
        let input = "var x = 10 + 20;";
        let mut lexer = Lexer::new(input.to_string());

        // var x = 10 + 20;
        assert_eq!(lexer.next_token(), Token::Var);
        assert_eq!(lexer.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Equals);
        assert_eq!(lexer.next_token(), Token::Number(10));
        assert_eq!(lexer.next_token(), Token::Plus);
        assert_eq!(lexer.next_token(), Token::Number(20));
        assert_eq!(lexer.next_token(), Token::Semicolon);
        assert_eq!(lexer.next_token(), Token::EOF);
    }

    #[test]
    fn test_lexer_if() {
        let input = "if (x < y) { y; } else if (x > y) { x; } else { 0; }";
        let mut lexer = Lexer::new(input.to_string());

        // if (x < y) { y } else if (x > y) { x } else { 0 }
        assert_eq!(lexer.next_token(), Token::If);
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Lower);
        assert_eq!(lexer.next_token(), Token::Identifier("y".to_string()));
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::LBrace);
        assert_eq!(lexer.next_token(), Token::Identifier("y".to_string()));
        assert_eq!(lexer.next_token(), Token::Semicolon);
        assert_eq!(lexer.next_token(), Token::RBrace);
        assert_eq!(lexer.next_token(), Token::Else);
        assert_eq!(lexer.next_token(), Token::If);
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Greater);
        assert_eq!(lexer.next_token(), Token::Identifier("y".to_string()));
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::LBrace);
        assert_eq!(lexer.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Semicolon);
        assert_eq!(lexer.next_token(), Token::RBrace);
        assert_eq!(lexer.next_token(), Token::Else);
        assert_eq!(lexer.next_token(), Token::LBrace);
        assert_eq!(lexer.next_token(), Token::Number(0));
        assert_eq!(lexer.next_token(), Token::Semicolon);
        assert_eq!(lexer.next_token(), Token::RBrace);
    }

    #[test]
    fn test_tokenize_variable() {
        let input = "var x = 10 + 20;";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();

        assert_eq!(
            tokens,
            vec![
                Token::Var,
                Token::Identifier("x".to_string()),
                Token::Equals,
                Token::Number(10),
                Token::Plus,
                Token::Number(20),
                Token::Semicolon,
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_tokenize_if() {
        let input = "if (x < y) { y; } else if (x > y) { x; } else { 0; }";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();

        assert_eq!(
            tokens,
            vec![
                Token::If,
                Token::LParen,
                Token::Identifier("x".to_string()),
                Token::Lower,
                Token::Identifier("y".to_string()),
                Token::RParen,
                Token::LBrace,
                Token::Identifier("y".to_string()),
                Token::Semicolon,
                Token::RBrace,
                Token::Else,
                Token::If,
                Token::LParen,
                Token::Identifier("x".to_string()),
                Token::Greater,
                Token::Identifier("y".to_string()),
                Token::RParen,
                Token::LBrace,
                Token::Identifier("x".to_string()),
                Token::Semicolon,
                Token::RBrace,
                Token::Else,
                Token::LBrace,
                Token::Number(0),
                Token::Semicolon,
                Token::RBrace,
                Token::EOF
            ]
        );
    }
}
