use crate::parser::{Expr, Op, Stmt};

pub struct Compiler {
    assembly: Vec<String>,
    label_counter: usize,
    variables: Vec<String>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            assembly: Vec::new(),
            label_counter: 0,
            variables: Vec::new(),
        }
    }

    pub fn declare_variable(&mut self, name: &str) {
        if !self.variables.contains(&name.to_string()) {
            self.variables.push(name.to_string());
        }
    }

    pub fn compile_program(&mut self, program: Vec<Stmt>) {
        for stmt in program {
            self.compile_stmt(stmt);
        }
    }

    pub fn compile_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::VarDeclaration(name, expr) => {
                self.declare_variable(&name);
                let value = self.compile_expr(expr);
                self.assembly.push(format!("mov dword [{}], {}", name, value));
            }
            Stmt::Expression(expr) => {
                let value = self.compile_expr(expr);
                self.assembly.push(format!("; evaluate {}", value));
            }
        }
    }

    pub fn compile_expr(&mut self, expr: Expr) -> String {
        match expr {
            Expr::Literal(value) => format!("{}", value),
            Expr::Variable(name) => format!("[{}]", name),
            Expr::BinaryOp(left, op, right) => {
                let left_value = self.compile_expr(*left);
                let right_value = self.compile_expr(*right);
                
                self.assembly.push(format!("mov rax, {}", left_value));
                
                if op == Op::Divide {
                    self.assembly.push(format!("mov rdx, 0"));
                    self.assembly.push(format!("div {}", right_value));
                } else {
                    self.assembly.push(format!("{} rax, {}", 
                    match op {
                        Op::Add => "add",
                        Op::Substract => "sub",
                        Op::Multiply => "imul",
                        _ => panic!("Invalid operator")
                    }, right_value));
                }

                "rax".to_string()
            }
        }
    }

    pub fn write_to_file(&self, filename: &str) {
        use std::fs::File;
        use std::io::Write;

        let mut file = File::create(filename).expect("Unable to create file");

        writeln!(file, "section .data").expect("Unable to write to file");
        for variable in self.variables.iter() {
            writeln!(file, "{} dd 0", variable).expect("Unable to write to file");
        }

        writeln!(file, "\nsection .text").expect("Unable to write to file");
        writeln!(file, "\tglobal _start").expect("Unable to write to file");
        writeln!(file, "\n_start:").expect("Unable to write to file");
        for line in &self.assembly {
            writeln!(file, "\t{}", line).expect("Unable to write to file");
        }

        writeln!(file, "\n\tmov rax, 60").expect("Unable to write to file");
        writeln!(file, "\txor rdi, rdi").expect("Unable to write to file");
        writeln!(file, "\tsyscall").expect("Unable to write to file");
    }

    pub fn get_assembly(&self) -> Vec<String> {
        self.assembly.clone()
    }

    pub fn get_variables(&self) -> Vec<String> {
        self.variables.clone()
    }

    fn new_label(&mut self, prefix: &str) -> String {
        let label = format!("{}{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }
}

#[cfg(test)]
mod test {
    use crate::{compiler::Compiler, parser::{Expr, Op, Stmt}};

    #[test]
    fn test_compile_var_declaration() {
        let mut compiler = Compiler::new();

        let stmt = Stmt::VarDeclaration("x".to_string(), Expr::Literal(10));
        compiler.compile_stmt(stmt);

        assert_eq!(compiler.get_variables(), vec!["x"]);
        assert_eq!(compiler.get_assembly(), vec!["mov [x], 10"]);
    }

    #[test]
    fn test_compile_expression() {
        let mut compiler = Compiler::new();

        let expr = Expr::BinaryOp(
            Box::new(Expr::Literal(10)),
            Op::Add,
            Box::new(Expr::Literal(20)),
        );
        compiler.compile_stmt(Stmt::Expression(expr));

        assert_eq!(compiler.get_assembly(), vec![
            "mov rax, 10",
            "add rax, 20",
            "; evaluate rax",
        ]);
    }
}