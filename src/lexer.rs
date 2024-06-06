/// Token represents the different types of tokens our lexer can produce
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Int,
    Return,
    Print,
    BracketO,
    BracketC,
    Identifier(String),
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    Number(f64),
    StringLiteral(String),
    Semicolon,
    String,
}

/// Lexer struct holds the input string and the current position in the string
pub struct Lexer {
    input: String,   // The input code to be tokenized
    position: usize, // The current position in the input string
}

impl Lexer {
    /// Constructor to create a new Lexer
    pub fn new(input: String) -> Self {
        Self { input, position: 0 }
    }

    /// Function to get the next token from the input
    pub fn get_next_token(&mut self) -> Option<Token> {
        // Loop until the end of the input
        while self.position < self.input.len() {
            let current_char = self.input.as_bytes()[self.position] as char;

            match current_char {
                // Skip whitespace characters
                ' ' | '\t' | '\n' | '\r' => {
                    self.position += 1;
                    continue;
                }
                // Handle numeric literals
                '0'..='9' => return Some(self.number()),
                // Handle the 'return' keyword
                'r' => {
                    if self.input[self.position..].starts_with("return") {
                        self.position += 6; // Move past "return"
                        return Some(Token::Return);
                    }
                }
                'p' => {
                    if self.input[self.position..].starts_with("print") {
                        self.position += 5;
                        return Some(Token::Print);
                    }
                }
                // Handle the 'Int' keyword
                'I' => {
                    if self.input[self.position..].starts_with("Int") {
                        self.position += 3; // Move past "Int"
                        return Some(Token::Int);
                    }
                }
                'S' => {
                    if self.input[self.position..].starts_with("String") {
                        self.position += 6;
                        return Some(Token::String);
                    }
                }
                '"' => {
                    self.position += 1; // Skip the opening quote
                    let start = self.position;
                    while self.position < self.input.len() && self.input.as_bytes()[self.position] as char != '"' {
                        self.position += 1;
                    }
                    if self.position < self.input.len() && self.input.as_bytes()[self.position] as char == '"' {
                        let string_content = self.input[start..self.position].to_string();
                        self.position += 1;
                        return Some(Token::StringLiteral(string_content));
                    } else {
                        panic!("Unterminated string literal");
                    }
                }
                // Handle identifiers
                'a'..='z' | 'A'..='Z' => {
                    let start = self.position;
                    while self.position < self.input.len()
                        && self.input.as_bytes()[self.position].is_ascii_alphanumeric()
                    {
                        self.position += 1;
                    }
                    let identifier = self.input[start..self.position].to_string();
                    return Some(Token::Identifier(identifier));
                }
                // Handle operators and symbols
                '=' => {
                    self.position += 1;
                    return Some(Token::Assign);
                }
                '+' => {
                    self.position += 1;
                    return Some(Token::Plus);
                }
                '-' => {
                    if self.position + 1 < self.input.len()
                        && self.input.as_bytes()[self.position + 1].is_ascii_digit()
                    {
                        return Some(self.number());
                    } else {
                        self.position += 1;
                        return Some(Token::Minus);
                    }
                }
                '*' => {
                    self.position += 1;
                    return Some(Token::Multiply);
                }
                '/' => {
                    self.position += 1;
                    return Some(Token::Divide);
                }
                '(' => {
                    self.position += 1;
                    return Some(Token::BracketO);
                }
                ')' => {
                    self.position += 1;
                    return Some(Token::BracketC);
                }
                ';' => {
                    self.position += 1;
                    return Some(Token::Semicolon);
                }
                // Unexpected character
                _ => panic!("Unexpected character: {}", current_char),
            }
        }

        None
    }

    /// Helper function to handle numeric literals
    fn number(&mut self) -> Token {
        let start = self.position;
        let mut has_decimal = false;
        let mut is_negative = false;

        if self.input.as_bytes()[self.position] as char == '-' {
            self.position += 1;
            is_negative = true;
        }

        while self.position < self.input.len()
            && (self.input.as_bytes()[self.position].is_ascii_digit()
                || self.input.as_bytes()[self.position] as char == '.')
        {
            if self.input.as_bytes()[self.position] as char == '.' {
                if has_decimal {
                    panic!("Invalid number format");
                }
                has_decimal = true;
            }
            self.position += 1;
        }

        let number_str = self.input[start..self.position].to_string();
        let number = if has_decimal {
            number_str.parse::<f64>().unwrap()
        } else {
            number_str.parse::<i32>().unwrap() as f64
        };
        Token::Number(if is_negative { -number } else { number })
    }
}