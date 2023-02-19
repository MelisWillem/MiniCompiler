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

    Space,
    EndOfLine,

    Number(i32), // just int's not floats at this point
    Word(String),
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

        TokenType::Space => String::from(" "),
        TokenType::EndOfLine => String::from("\n"),

        TokenType::Number(i) => i.to_string(),
        TokenType::Word(s) => s.to_string(),
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
