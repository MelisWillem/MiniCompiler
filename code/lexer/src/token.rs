#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TokenType {
    Func,
    Struct,
    If,
    Else,
    Var,
    Assign,

    Plus,
    Minus,
    Mul,
    Div,
    Equal,

    LeftParen,
    RightParen,
    LeftSquareBrackets,
    RightSquareBrackets,
    LeftCurlyBrackets,
    RightCurlyBrackets,

    SmallerThen,
    GreaterThen,

    Space,
    EndOfLine,

    Number(i32), // just int's not floats at this point
    Identifier(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Associativity{
    Left, 
    Right
}

impl TokenType {
    pub fn precedence(&self) -> i32 {
        match self {
            TokenType::SmallerThen => 0,
            TokenType::GreaterThen => 0,
            TokenType::Plus => 20,
            TokenType::Minus => 20,
            TokenType::Mul => 40,
            TokenType::Div => 40,
            _ => -1,
        }
    }

    pub fn associativity(&self) -> Associativity{
        Associativity::Left
    }

    pub fn is_binary_operator(&self) -> bool {
        match self {
            TokenType::SmallerThen => true,
            TokenType::GreaterThen => true,
            TokenType::Plus => true,
            TokenType::Minus => true,
            TokenType::Mul => true,
            TokenType::Div => true,
            _ => false,
        }
    }
}

pub fn to_string(t: &TokenType) -> String {
    match t {
        TokenType::Func => String::from("func"),
        TokenType::Struct => String::from("struct"),
        TokenType::If => String::from("if"),
        TokenType::Else => String::from("else"),
        TokenType::Var => String::from("var"),

        TokenType::Plus => String::from("+"),
        TokenType::Minus => String::from("-"),
        TokenType::Mul => String::from("*"),
        TokenType::Div => String::from("/"),
        TokenType::Assign => String::from("="),
        TokenType::Equal => String::from("=="),

        TokenType::LeftParen => String::from("("),
        TokenType::RightParen => String::from(")"),
        TokenType::LeftSquareBrackets => String::from("["),
        TokenType::RightSquareBrackets => String::from("]"),
        TokenType::LeftCurlyBrackets => String::from("{"),
        TokenType::RightCurlyBrackets => String::from("{"),

        TokenType::SmallerThen => String::from("<"),
        TokenType::GreaterThen => String::from(">"),

        TokenType::Space => String::from(" "),
        TokenType::EndOfLine => String::from("\n"),

        TokenType::Number(i) => i.to_string(),
        TokenType::Identifier(s) => s.to_string(),
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub file: std::path::PathBuf,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        file: std::path::PathBuf,
        line: usize,
        column: usize,
    ) -> Token {
        Token {
            token_type,
            file,
            line,
            column,
        }
    }
}
