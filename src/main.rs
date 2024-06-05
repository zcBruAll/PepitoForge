use std::env;
use std::fs;
use std::process::Command;

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
    DoubleQuote,
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

/// ASTNode represents the different types of nodes in our Abstract Syntax Tree
#[derive(Debug)]
pub enum ASTNode {
    Return(Box<Expression>),
    Content(Vec<Box<Expression>>),
    VariableDeclaration(Vec<(String, String, Box<Expression>)>),
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
    String(String),
    Identifier(String),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>),
    Print(Box<Expression>),
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
                    let expr = self.expect_operand();
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

/// CodeGenerator struct handles the generation of assembly code from the AST
pub struct CodeGenerator;

impl CodeGenerator {
    /// Function to generate assembly code from the AST
    pub fn generate(ast: &ASTNode) -> String {
        match ast {
            ASTNode::Return(value) => {
                format!(
                    "\txor rdi, rdi\n\
                    \tmov rax, 60\n\
                    \tsyscall"
                )
            }, // Generate the assembly code to exit with the return value
            ASTNode::VariableDeclaration(variables) => {
                let bss_section = variables
                    .iter()
                    .map(|(var_name, var_type, value)| {
                        CodeGenerator::generate_variable_declaration(var_name, var_type, value)
                    })
                    .collect::<Vec<String>>()
                    .join("\n");
                format!(
                    "\nsection.bss\n\
                    \tbuffer resb 21  ; Reserve 11 bytes for the largest 32-bit integer\n\
                    {}\n",
                    bss_section
                )
            }
            ASTNode::Content(values) => {
                let text_section = values
                    .iter()
                    .map(|expr| CodeGenerator::generate_expression(expr))
                    .collect::<Vec<String>>()
                    .join("\n");
                format!(
                    "\nsection .text\n\
                    global _start\n\
                    extern int_to_string\n\n\
                    \
                    _start:\n\
                    {}\n",
                    text_section
                )
            }
        }
    }

    fn generate_return(value: &Expression) -> String {
        format!(
            "\tmov rax, 60  ; exit\n\
            \tmov rbx, {}\n\
            \tsyscall",
            CodeGenerator::generate_expression(value)
        )
    }

    fn generate_variable_declaration(var_name: &str, var_type: &str, value: &Expression) -> String {
        format!(
            "\t{} {} {}",
            var_name,
            var_type,
            CodeGenerator::generate_expression(value)
        )
    }

    fn generate_expression(expr: &Expression) -> String {
        match expr {
            Expression::Integer(n) => n.to_string(),
            Expression::Float(n) => n.to_string(),
            Expression::String(s) => format!("'{}'", s),
            Expression::Identifier(name) => format!("[{}]", name.clone()),
            Expression::BinaryOperation(left, op, right) => {
                let left_expr = CodeGenerator::generate_expression(left);
                let right_expr = CodeGenerator::generate_expression(right);
                let op_str = match op {
                    Operator::Add => "add",
                    Operator::Subtract => "sub",
                    Operator::Multiply => "mul",
                    Operator::Divide => "div",
                };
                match op {
                    Operator::Add | Operator::Subtract => {
                        format!(
                            "\tmov rax, {}\n\
                            \t{} rax, {}\n\
                            \tmov rdi, rax\n",
                            left_expr, op_str, right_expr
                        )
                    }
                    Operator::Multiply | Operator::Divide => {
                        format!(
                            "\tmov rax, {}\n\
                            \tmov rbx, {}\n\
                            \t{} rbx\n\
                            \tmov rdi, rax\n",
                            left_expr, right_expr, op_str
                        )
                    }
                }
            }
            Expression::Print(to_print) => {
                let to_print_expr = CodeGenerator::generate_expression(to_print);
                format!(
                    "{}\n\
                    \tlea rsi, [buffer + 20]\n\
                    \tcall int_to_string\n\n\
                    \
                    \tlea rdi, [buffer + 20]\n\
                    \tsub rdi, rsi\n\
                    \tsub rsi, rdi\n\n\
                    \
                    \tmov rax, 4            ; Write in console\n\
                    \tmov rdi, 1\n\
                    \tmov rdx, 21\n\
                    \tsyscall\n",
                    to_print_expr
                )
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

    let mut asm_to_compile: Vec<String> = Vec::new();
    asm_to_compile.push("output.asm".to_string());
    asm_to_compile.push("int_to_string.asm".to_string());

    fs::write(asm_to_compile[0].clone(), assembly).expect("Could not write assembly file");

    // Assemble the code using nasm
    for asm_file in &asm_to_compile {
        let output = Command::new("nasm")
            .args(&["-felf64", &asm_file])
            .output()
            .expect("Failed to execute nasm");
        if !output.status.success() {
            eprintln!("nasm failed: {}", String::from_utf8_lossy(&output.stderr));
            std::process::exit(1);
        }
    }

    let args = vec!["-o", "output", "output.o", "int_to_string.o"];
    let output = Command::new("ld")
        .args(&args)
        .output()
        .expect("Failed to execute ld");
    if !output.status.success() {
        eprintln!("ld failed: {}", String::from_utf8_lossy(&output.stderr));
        std::process::exit(1);
    }

    println!("Executable created: output");
}
