use crate::ast::{ASTNode, Expression, Operator};

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
                    "section .data\n\
                    \tnewline db 0xA\n\
                    {}\n\n\
                    section .bss\n\
                    \tbuffer resb 20\n",
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
                    \textern int_to_string\n\
                    \tglobal _start\n\n\
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
            "\t{} {} {}{}",
            var_name,
            var_type,
            CodeGenerator::generate_expression(value),
            if var_type == "db" {
                format!(
                    "\n\
                    \tlen_{} equ $ - {}",
                    var_name, var_name
                )
            } else {
                format!("")
            }
        )
    }

    fn generate_expression(expr: &Expression) -> String {
        match expr {
            Expression::Integer(n) => n.to_string(),
            Expression::Float(n) => n.to_string(),
            Expression::String(s) => format!("'{}'", s),
            Expression::Identifier(name) => format!("{}", name.clone()),
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
                            "\tmov rax, [{}]\n\
                            \t{} rax, [{}]\n",
                            left_expr, op_str, right_expr
                        )
                    }
                    Operator::Multiply | Operator::Divide => {
                        format!(
                            "\tmov rax, [{}]\n\
                            \tmov rbx, [{}]\n\
                            \t{} rbx\n",
                            left_expr, right_expr, op_str
                        )
                    }
                }
            }
            Expression::Print(to_print) => {
                let to_print_expr = CodeGenerator::generate_expression(to_print);
                format!(
                    "{}\n\
                    \tlea rdi, [buffer]\n\n\
                    \tcall int_to_string\n\n\
                    \tmov rax, 1            ; Write in console\n\
                    \tmov rdi, 1\n\
                    \tlea rsi, [buffer]\n\
                    \tlea rdx, [buffer + 20]\n\
                    \tsub rdx, rsi\n\
                    \tsyscall\n\n\
                    \
                    \tmov rax, 1\n\
                    \tmov rdi, 1\n\
                    \tmov rsi, newline\n\
                    \tmov rdx, 1\n\
                    \tsyscall\n",
                    to_print_expr
                )
            }
        }
    }
}