pub mod ast;

use std::{borrow::Borrow, fmt::format, ops::Deref, path::PathBuf};

use ast::*;
use lexer::token::{Associativity, Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn is_relevant(t: &Token) -> bool {
        t.token_type != TokenType::Space && t.token_type != TokenType::EndOfLine
    }

    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens.into_iter().filter(Parser::is_relevant).collect(),
            current: 0,
        }
    }

    fn error(&mut self, message: &str) {
        // Dirty hack to get going
        panic!("{message}");
    }

    fn peek(&self) -> Option<&Token> {
        if self.current >= self.tokens.len() {
            return None;
        }
        Some(&self.tokens[self.current])
    }

    fn peek_next(&self) -> Option<&Token> {
        let next = self.current + 1;

        if next >= self.tokens.len() {
            return None;
        }
        Some(&self.tokens[next])
    }

    pub fn consume(&mut self) -> Option<Token> {
        if self.current >= self.tokens.len() {
            return None;
        }
        self.current = self.current + 1;
        Some(self.tokens[self.current - 1].clone())
    }

    // Not sure if this is a good idea.
    pub fn undo_consume(&mut self) -> bool {
        if self.current <= 0 {
            return false;
        }
        self.current = self.current - 1;
        return true;
    }

    fn peek_is_token_type(&self, tok_type: TokenType) -> bool {
        if let Some(t) = self.peek() {
            return t.token_type == tok_type;
        }

        false
    }

    fn consume_tokentype(&mut self, tok_type: TokenType) -> Option<Token> {
        // This is sometimes problematic as TokenType can contain values
        // in which case we can't provide it as an argument here as we don't
        // know the value of it's values.
        if let Some(tok) = self.peek() {
            if tok.token_type == tok_type {
                Some(self.consume());
            }
        }
        None
    }

    fn identifier(&mut self) -> Option<String> {
        if let Some(tok) = self.consume() {
            return match tok.token_type {
                TokenType::Identifier(s) => Some(s),
                _ => {
                    self.undo_consume();
                    None
                }
            };
        }
        None
    }

    fn number(&mut self) -> Option<LiteralExpr> {
        if let Some(t) = self.consume() {
            return match t.token_type {
                TokenType::Number(n) => Some(LiteralExpr {
                    constant_type: BuildinTypeKind::Int,
                    value: n,
                }),
                _ => {
                    self.undo_consume();
                    None
                }
            };
        }

        None
    }

    fn number_or_identifier(&mut self) -> Option<ExprKind> {
        // get the first identifier
        if let Some(id) = self.identifier() {
            return Some(ExprKind::Identifier(id));
        } else if let Some(number) = self.number() {
            return Some(ExprKind::Literal(number));
        } else {
            let wrong_token = self.peek();
            let error_message = format!(
                "Failed to parse expression: expected literal or number but got token {:?}.",
                wrong_token
            );
            self.error(&error_message);
            return None;
        }
    }

    fn token_type_to_binary_op(token_type: TokenType) -> Option<BinaryOperatorKind> {
        match token_type {
            TokenType::Plus => Some(BinaryOperatorKind::Plus),
            TokenType::Minus => Some(BinaryOperatorKind::Minus),
            TokenType::Mul => Some(BinaryOperatorKind::Mul),
            TokenType::Div => Some(BinaryOperatorKind::Div),
            _ => None,
        }
    }

    fn buildOperator(operator_token: Token, lhs: ExprKind, rhs: ExprKind) -> Option<ExprKind> {
        match operator_token.token_type {
            t if t.is_binary_operator() => Some(ExprKind::Binary(BinaryExpr {
                operator: Parser::token_type_to_binary_op(t).unwrap(),
                left: Box::new(lhs),
                right: Box::new(rhs),
            })),
            _ => None,
        }
    }

    fn expr(&mut self, current_precendence: Option<i32>) -> Option<ExprKind> {
        // higher precendence is more sticky...
        let current_precendence = current_precendence.unwrap_or(-1);

        // https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
        // We should alway be able to get the identifier/number if this is an expression.
        let mut lhs = self.number_or_identifier();

        while self.current < self.tokens.len() {
            let operator_token = match self.peek() {
                Some(t)
                    if t.token_type.is_binary_operator()
                        // Only enter the loop is the operator has higher,
                        // as higher precedence needs to be evaluated first.
                        && t.token_type.precedence() > current_precendence =>
                {
                    self.consume()
                }
                other => {
                    println!(
                        "Not an operator, token={:?}, peek()={:?}.",
                        other,
                        self.peek()
                    );
                    None
                }
            };

            if let Some(operator_token) = operator_token {
                let precedence = operator_token.token_type.precedence();

                // assuming left associativity:
                // If the next operator has the same precendence as this one.
                // Then we want to evaluate the current operator first, and then
                // the next operator.
                let next_precendence =
                    if Associativity::Left == operator_token.token_type.associativity() {
                        precedence + 1
                    } else {
                        precedence
                    };

                let rhs = self.expr(Some(next_precendence)).unwrap();
                lhs = Some(Parser::buildOperator(operator_token, lhs.unwrap(), rhs).unwrap());
            } else {
                // if no more binary operators -> exit
                return lhs;
            }
        }

        lhs
    }

    fn stmt(&mut self) -> Option<StmtKind> {
        // examples of statements
        // i = i + 1;
        // a = 5;
        // b = a;
        // -> The lhs is a identifier -> must be word token.
        // -> The rhs is an expression -> expr()
        if let Some(left) = self.consume() {
            match left.token_type {
                TokenType::Identifier(name) => {
                    if let Some(right) = self.expr(None) {
                        // TODO create statement
                    } else {
                        self.error("Unable to read rhs of expression.")
                    }
                }
                _ => {
                    self.error("Should be identifier.");
                }
            }
        } else {
            self.error("Unable to parse left side of statement, invalid expression.");
        }

        None
    }

    fn bblock(&mut self) -> Option<Ast>{
        let mut statements: Vec<StmtKind> = Vec::new();

        if let Some(t) = self.consume() {
            match t.token_type {
                TokenType::RightSquareBrackets => {}
                _ => 
                {
                    self.error("Missing open brackets on basic block.");
                    return None;
                } // early exit not open bracket
            }
        }

        loop {
            if let Some(t) = self.consume(){
                let ast = match t.token_type{
                    TokenType::LeftCurlyBrackets => self.bblock(),
                    TokenType::RightCurlyBrackets => None,
                    _ => {
                        let err_msg = format!("unexpected token {:?}", t);
                        self.error(&err_msg);
                        return None;
                    }
                };
            }

            return None;
        }
    }

    fn decl(&mut self) -> Option<ExprKind> {
        self.consume_tokentype(TokenType::Var);
        if let Some(identifier) = self.identifier() {
            Some(ExprKind::Decl(identifier))
        } else {
            self.error("Expected identifier name");
            None
        }
    }

    fn call(&mut self) -> Option<ExprKind> {
        None
    }

    pub fn parse(&mut self) -> Option<Ast> {
        loop {
            if let Some(t) = self.peek() {
                match t.token_type.borrow() {
                    // Top level can only be a struct or a function at this point.
                    TokenType::Func => self.decl(),
                    TokenType::Struct => self.decl(),
                    _ => {
                        // unknown symbol
                        break;
                    }
                };
            }
        }

        None // TODO
    }
}

fn buildConst(value: i32) -> Box<ExprKind> {
    Box::<ExprKind>::new(ExprKind::Literal(LiteralExpr {
        constant_type: BuildinTypeKind::Int,
        value: value,
    }))
}

fn buildMul(left: Box<ExprKind>, right: Box<ExprKind>) -> Box<ExprKind> {
    let bin = BinaryExpr {
        operator: BinaryOperatorKind::Mul,
        left: left,
        right: right,
    };
    Box::<ExprKind>::new(ExprKind::Binary(bin))
}

fn buildSum(left: Box<ExprKind>, right: Box<ExprKind>) -> Box<ExprKind> {
    let bin = BinaryExpr {
        operator: BinaryOperatorKind::Plus,
        left: left,
        right: right,
    };
    Box::<ExprKind>::new(ExprKind::Binary(bin))
}

#[test]
fn expressions() {
    let test_data = vec![("2+3*4", "(+ 2 (* 3 4))"), ("2*3+4", "(+ (* 2 3) 4)")];

    for (str_expression, expected_parsed_expression) in test_data {
        let lexer_res = lexer::Lexer::new(PathBuf::new(), str_expression.to_owned()).scan();
        let expr = Parser::new(lexer_res.tokens).expr(None);

        assert!(expr.is_some());
        let expr = expr.unwrap().to_string();

        println!("parsed expression {}", expr);
        assert_eq!(expr, expected_parsed_expression);
    }
}
