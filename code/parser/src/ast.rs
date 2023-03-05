use std::rc::Rc;

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
    pub left: Rc<ExprKind>,
    pub right: Rc<ExprKind>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum OperatorAssociativity {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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
    CallExpr {
        callee: String,
        args: Vec<ExprKind>,
    }
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
            ExprKind::CallExpr { callee, args } => {
                let mut args_str = String::new();
                for arg in args {
                    args_str.push_str(&arg.to_string());
                }
                format!("{}({})", callee, args_str)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignExpr {
    pub left: Box<ExprKind>,
    pub right: Box<ExprKind>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Ast {
    Assign(AssignExpr),
    ReturnStatement(Option<ExprKind>),
    VarDecl {
        name: String,
        rhs: ExprKind,
    },
    IfStmt {
        condition: Expr,
        code: Box<Vec<Ast>>,
        else_code: Option<Box<Vec<Ast>>>,
    },
    BasicBlock {
        statements: Vec<Box<Ast>>,
    },
    FunDecl {
        name: String,
        args: Vec<FunArg>,
        returns: UnresolvedType,
        implementation: Option<Box<Ast>>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnresolvedType {
    pub name: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunArg {
    pub name: String,
    pub arg_type: UnresolvedType,
}

impl FunArg {
    pub fn to_string(&self) -> String {
        format!("(arg {0} {1})", self.arg_type.name, self.name).to_owned()
    }
}
