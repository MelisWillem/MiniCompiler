#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperatorKind {
    Plus,
    Minus,
    Mul,
    Div,
    SmallerThen,
    GreaterThen,
}

impl BinaryOperatorKind {
    pub fn to_string(&self) -> String {
        match &self {
            BinaryOperatorKind::Plus => "+",
            BinaryOperatorKind::Minus => "-",
            BinaryOperatorKind::Mul => "*",
            BinaryOperatorKind::Div => "/",
            BinaryOperatorKind::SmallerThen => "<",
            BinaryOperatorKind::GreaterThen => ">",
        }
        .to_owned()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinaryExpr {
    pub operator: BinaryOperatorKind,
    pub left: Box<ExprKind>,
    pub right: Box<ExprKind>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum OperatorAssociativity {
    Left,
    Right,
}

impl BinaryExpr {
    fn precedence(&self) -> i32 {
        match self.operator {
            BinaryOperatorKind::SmallerThen => 0,
            BinaryOperatorKind::GreaterThen => 0,
            BinaryOperatorKind::Plus => 20,
            BinaryOperatorKind::Minus => 20,
            BinaryOperatorKind::Mul => 40,
            BinaryOperatorKind::Div => 40,
        }
    }

    fn associativity(&self) -> OperatorAssociativity {
        match self.operator {
            BinaryOperatorKind::SmallerThen => OperatorAssociativity::Left,
            BinaryOperatorKind::GreaterThen => OperatorAssociativity::Left,
            BinaryOperatorKind::Plus => OperatorAssociativity::Left,
            BinaryOperatorKind::Minus => OperatorAssociativity::Left,
            BinaryOperatorKind::Mul => OperatorAssociativity::Left,
            BinaryOperatorKind::Div => OperatorAssociativity::Left,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BuildinTypeKind {
    Int,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AggregateField {}

#[derive(Debug, PartialEq, Eq)]
pub struct AggregateType {
    pub name: String,
    pub fields: Vec<AggregateField>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeKind {
    Buildin(BuildinTypeKind),
    Aggregate(AggregateType),
}

#[derive(Debug, PartialEq, Eq)]
pub struct LiteralExpr {
    pub constant_type: BuildinTypeKind,
    pub value: i32, // Only one kind of constant at this time.
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    kind: ExprKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    Identifier(String),
    Literal(LiteralExpr),
    Binary(BinaryExpr),
    Decl(String),
}

impl ExprKind {
    pub fn to_string(&self) -> String {
        match self {
            ExprKind::Identifier(s) => s.clone(),
            ExprKind::Literal(lit_expr) => format!("{}", lit_expr.value),
            ExprKind::Binary(bin_expr) => format!(
                "({} {} {})",
                bin_expr.operator.to_string(),
                bin_expr.left.to_string(),
                bin_expr.right.to_string()
            ),
            ExprKind::Decl(name) => name.to_owned(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignExpr {
    pub left: Box<ExprKind>,
    pub right: Box<ExprKind>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    Assign(AssignExpr),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Ast {
    Expr(ExprKind),
    BasicBlock(),
    Stmt(StmtKind),
}
