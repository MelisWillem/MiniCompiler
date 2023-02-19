use lexer::token::Token;

pub enum BinaryOperatorKind {
    Plus,
    Minus,
    Mul,
    Div,
}

pub struct BinaryExpr {
    pub operator: BinaryOperatorKind,
    pub left: Box<ExprKind>,
    pub right: Box<ExprKind>,
}

pub enum BuildinTypeKind {
    Int,
}

pub struct AggregateField {}

pub struct AggregateType {
    pub name: String,
    pub fields: Vec<AggregateField>,
}

pub enum TypeKind {
    Buildin(BuildinTypeKind),
    Aggregate(AggregateType),
}

pub struct ConstantExpr {
    pub constant_type: BuildinTypeKind,
    pub value: i32, // Only one kind of constant at this time.
}

pub enum ExprKind {
    Unary(ConstantExpr),
    Binary(BinaryExpr),
}

pub struct AssignExpr {
    pub left: Box<ExprKind>,
    pub right: Box<ExprKind>,
}

pub enum StmtKind {
    Assign(AssignExpr),
}

pub enum Ast {
    Expr(ExprKind),
    Stmt(StmtKind)
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {tokens, current: 0}
    }

    pub fn Parse(& mut self) -> Option<Ast> {
        None // TODO
    }
}