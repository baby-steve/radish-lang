use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(f64),
}

impl From<Literal> for String {
    fn from(ast: Literal) -> String {
        match ast {
            Literal::Number(val) => val.to_string(),
        }
    }
}


#[derive(Debug, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub left: AST,
    pub op: Op,
    pub right: AST,
}

impl fmt::Display for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ left: {:?}, op: {:?}, right: {:?}, }}", self.left, self.op, self.right)
    }
}

#[derive(Debug, PartialEq)]
pub enum AST {
    BinaryExpr(Box<BinaryExpr>),
    Literal(Literal),
}

#[derive(Debug, PartialEq)]
pub struct File {
    pub items: Vec<AST>,
}
