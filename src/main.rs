use std::env;
use std::fs;
use std::process::Command;

/// Token represents the different types of tokens our lexer can produce
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Int,
    Return, // 'return' keyword
    Identifier(String),
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    Number(f64), // Numeric literal
    Semicolon,   // ';' character
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
                // Handle the 'Int' keyword
                'I' => {
                    if self.input[self.position..].starts_with("Int") {
                        self.position += 3; // Move past "Int"
                        return Some(Token::Int);
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
                    if self.position + 1 < self.input.len() && self.input.as_bytes()[self.position + 1].is_ascii_digit() {
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

/// ASTNode represents the different types of nodes in our Abstract Syntax Tree
#[derive(Debug)]
pub enum ASTNode {
    Return(Vec<Box<Expression>>), // Return statement with a numeric value
    VariableDeclaration(Vec<(String, Box<Expression>)>),
}

#[derive(Debug)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// Expression represents various expressions in our language
#[derive(Debug)]
pub enum Expression {
    Integer(i32),
    Float(f64),
    Identifier(String),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>),
}

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
        let mut variables: Vec<(String, Box<Expression>)> = Vec::new();
        while let Some(token) = self.tokens.get(self.position) {
            match token {
                Token::Return => {
                    self.position += 1;
                    let expr = self.expect_expression();
                    self.expect_token(Token::Semicolon); // Expect a semicolon
                    text.push(Box::new(expr));
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
                    variables.push((var_name, Box::new(value)));
                }
                _ => panic!("Unexpected token"),
            }
        }

        ast.push(ASTNode::VariableDeclaration(variables));
        ast.push(ASTNode::Return(text));
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
            },
            Token::Identifier(name) => Expression::Identifier(name),
            _ => panic!("Expected a number or identifier"),
        }
    }
}

/// CodeGenerator struct handles the generation of assembly code from the AST
pub struct CodeGenerator;

impl CodeGenerator {
    /// Function to generate assembly code from the AST
    pub fn generate(ast: &ASTNode) -> String {
        match ast {
            ASTNode::Return(values) => {
                let text_section = values
                    .iter()
                    .map(|value| {
                        CodeGenerator::generate_return(value)
                    })
                    .collect::<Vec<String>>()
                    .join("\n");
                format!(
                    "section.text\n\
                    global _start\n\n\
                    \
                    _start:\n\
                    {}",
                    text_section
                )
            } // Generate the assembly code to exit with the return value
            ASTNode::VariableDeclaration(variables) => {
                let bss_section = variables
                    .iter()
                    .map(|(var_name, value)| {
                        CodeGenerator::generate_variable_declaration(var_name, value)
                    })
                    .collect::<Vec<String>>()
                    .join("\n");
                format!(
                    "section.bss\n\
                    {}\n\n",
                    bss_section
                )
            }
        }
    }

    fn generate_return(value: &Expression) -> String {
        format!(
            "{}\n\
            \tmov eax, 60  ; syscall: exit\n\
            \tsyscall\n",
            CodeGenerator::generate_expression(value)
        )
    }

    fn generate_variable_declaration(var_name: &str, value: &Expression) -> String {
        format!(
            "\t{} dd {}",
            var_name,
            CodeGenerator::generate_expression(value)
        )
    }

    fn generate_expression(expr: &Expression) -> String {
        match expr {
            Expression::Integer(n) => n.to_string(),
            Expression::Float(n) => n.to_string(),
            Expression::Identifier(name) => format!("[{}]", name.clone()),
            Expression::BinaryOperation(left, op, right) => {
                let left_expr = CodeGenerator::generate_expression(left);
                let right_expr = CodeGenerator::generate_expression(right);
                let op_str = match op {
                    Operator::Add => "add",
                    Operator::Subtract => "sub",
                    Operator::Multiply => "mul",
                    Operator::Divide => "fdiv",
                };
                match op {
                    Operator::Add | Operator::Subtract => {
                        format!(
                            "\tmov eax, {}\n\
                            \t{} eax, {}\n\
                            \tmov edi, eax",
                            left_expr, op_str, right_expr
                        )
                    },
                    Operator::Multiply | Operator::Divide => {
                        format!(
                            "\tmov eax, {}\n\
                            \tmov ebx, {}\n\
                            \t{} ebx\n\
                            \tmov edi, eax",
                            left_expr, right_expr, op_str
                        )
                    }
                }
            }
        }
    }
}

fn main() {
    // Get command-line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename.pc>", args[0]);
        std::process::exit(1);
    }

    // Read the file content
    let filename = &args[1];
    let input = fs::read_to_string(filename).expect("Could not read file");

    // Create a new lexer with the input code
    let mut lexer = Lexer::new(input);
    // Tokenize the input code
    let mut tokens = Vec::new();
    while let Some(token) = lexer.get_next_token() {
        tokens.push(token);
    }

    // Create a new parser with the tokens
    let mut parser = Parser::new(tokens);
    // Parse the tokens into an AST
    let ast = parser.parse();
    println!("{:?}", ast);

    // Generate assembly code from the AST
    let mut assembly = String::new();
    for node in &ast {
        assembly.push_str(&CodeGenerator::generate(node));
    }
    println!("{}", assembly);

    // Write the assembly code to a file
    let asm_filename = "output.asm";
    fs::write(asm_filename, assembly).expect("Could not write assembly file");

    // Assemble the code using nasm
    let output = Command::new("nasm")
        .args(&["-felf64", asm_filename])
        .output()
        .expect("Failed to execute nasm");
    if !output.status.success() {
        eprintln!("nasm failed: {}", String::from_utf8_lossy(&output.stderr));
        std::process::exit(1);
    }

    // Link the object file to create the executable
    let obj_filename = "output.o";
    let output = Command::new("ld")
        .args(&["-o", "output", obj_filename])
        .output()
        .expect("Failed to execute ld");
    if !output.status.success() {
        eprintln!("ld failed: {}", String::from_utf8_lossy(&output.stderr));
        std::process::exit(1);
    }

    println!("Executable created: output");
}
