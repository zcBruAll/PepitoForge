mod lexer;
mod parser;
mod code_gen;
mod ast;

use std::env;
use std::fs;
use std::process::Command;

use lexer::Lexer;
use parser::Parser;
use code_gen::CodeGenerator;

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
