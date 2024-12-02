use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;

pub mod lexer;
pub mod parser;
pub mod compiler;

fn main() {
    let input = "
        var x = 10;
        var y = 20;
        var z = x + y;
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
    let stmts = parser.parse_program();

    let mut compiler = Compiler::new();
    compiler.compile_program(stmts);
    compiler.write_to_file("output.asm");
}
