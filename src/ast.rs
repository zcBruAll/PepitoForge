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