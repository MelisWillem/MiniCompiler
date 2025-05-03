pub mod ast;

use ast::*;
use lexer::token::{Associativity, Token, TokenType};
use std::borrow::Borrow;
use std::rc::Rc;

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

    fn error_owned(&mut self, message: String) {
        // Dirty hack to get going
        panic!("{message}");
    }

    fn peek(&self) -> Option<&Token> {
        if self.current >= self.tokens.len() {
            return None;
        }
        Some(&self.tokens[self.current])
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

    fn consume_tokentype(&mut self, tok_type: TokenType) -> Option<Token> {
        // This is sometimes problematic as TokenType can contain values
        // in which case we can't provide it as an argument here as we don't
        // know the value of it's values.
        if let Some(tok) = self.peek() {
            if tok.token_type == tok_type {
                return self.consume();
            }
        }
        return None;
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

    fn build_operator(operator_token: Token, lhs: ExprKind, rhs: ExprKind) -> Option<ExprKind> {
        match operator_token.token_type {
            t if t.is_binary_operator() => Some(ExprKind::Binary(BinaryExpr {
                operator: Parser::token_type_to_binary_op(t).unwrap(),
                left: Rc::new(lhs),
                right: Rc::new(rhs),
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
                Some(t) if t.token_type == TokenType::SemiColon => {
                    return lhs; // exit the expression evaluation
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
                lhs = Some(Parser::build_operator(operator_token, lhs.unwrap(), rhs).unwrap());
            } else {
                // if no more binary operators -> exit
                return lhs;
            }
        }

        lhs
    }

    fn expr_stmt(&mut self) -> Option<Ast> {
        if let Some(lhs) = self.expr(None) {
            if let Some(seperator) = self.peek() {
                match seperator.token_type {
                    TokenType::Assign => {
                        self.consume();
                        if let Some(rhs) = self.expr(None) {
                            return Some(Ast::Assign(AssignExpr {
                                left: Box::new(lhs),
                                right: Box::new(rhs),
                            }));
                        }
                    }
                    TokenType::SemiColon => {
                        self.consume(); // consume the ;
                    }
                    _ => {
                        self.error_owned(format!("Error can't parse expression statement, expected '=' or ';' but got {:?}", seperator));
                    }
                }
            }
        }
        None
    }

    fn if_stmt(&mut self) -> Option<Ast> {
        None
    }

    fn return_stmt(&mut self) -> Ast {
        self.consume_tokentype(TokenType::Return);
        if self.consume_tokentype(TokenType::SemiColon).is_some() {
            return Ast::ReturnStatement(None);
        }
        let expr = self.expr(None);
        if self.consume_tokentype(TokenType::SemiColon).is_none() {
            self.error("Expected semi colon after return statement");
        }
        return Ast::ReturnStatement(expr);
    }

    fn stmt(&mut self) -> Option<Ast> {
        if let Some(t) = self.peek() {
            return match &t.token_type {
                TokenType::If => self.if_stmt(),
                TokenType::Return => Some(self.return_stmt()),
                _ => return self.expr_stmt(),
            };
        }
        None
    }

    fn bblock(&mut self) -> Option<Box<Ast>> {
        if let Some(t) = self.consume() {
            match t.token_type {
                TokenType::LeftCurlyBrackets => {}
                _ => {
                    self.error_owned(format!("Missing open brackets on basic block, got {:?}", t));
                    return None;
                } // early exit not open bracket
            }
        }

        let mut statements: Vec<Box<Ast>> = Vec::new();
        loop {
            if let Some(t) = self.peek() {
                let maybe_statement = match t.token_type {
                    TokenType::RightCurlyBrackets => {
                        self.consume();
                        None
                    }
                    _ => self.stmt(),
                };

                if let Some(stmt) = maybe_statement {
                    statements.push(Box::new(stmt));
                } else {
                    break;
                }
            }
        }
        return Some(Box::new(Ast::BasicBlock { statements }));
    }

    fn var_decl(&mut self) -> Option<Ast> {
        self.consume_tokentype(TokenType::Var);
        if let Some(name) = self.identifier() {
            if let Some(rhs) = self.expr(None) {
                return Some(Ast::VarDecl { name, rhs });
            } else {
                self.error(format!("no rhs on variable declarion of {0}", name).as_str());
            }
            None
        } else {
            self.error("Expected identifier name");
            None
        }
    }

    fn func_decl(&mut self) -> Option<Ast> {
        if let None = self.consume_tokentype(TokenType::Func {}) {
            // string format
            self.error_owned(format!("Expected func declaration, got {:?}", self.peek()));
            return None;
        }

        if let None = self.identifier() {
            self.error_owned(format!("Expected function name"));
        }

        if let None = self.consume_tokentype(TokenType::LeftParen {}) {
            self.error("Expected left paren to define the arguments of the function.")
        }

        let mut args: Vec<FunArg> = vec![];
        loop {
            if let Some(name) = self.identifier() {
                if let None = self.consume_tokentype(TokenType::Colon {}) {
                    self.error("Colon missing between argument identifier and type.");
                }
                if let Some(type_name) = self.identifier() {
                    let arg_type = UnresolvedType { name: type_name };
                    args.push(FunArg { name, arg_type });
                } else {
                    self.error("Expected type name.")
                }
            } else {
                break;
            }

            if let None = self.consume_tokentype(TokenType::Comma) {
                // No more arguments, so break the loop.
                break;
            }
        }

        if let None = self.consume_tokentype(TokenType::RightParen) {
            self.error("Expected closing parent of function arguments");
        }

        // Try to consume arrow or left curly brackets
        let mut return_type = UnresolvedType {
            name: "void".to_owned(),
        }; // TODO!!
        if let Some(maybe_arrow) = self.peek() {
            if maybe_arrow.token_type == TokenType::Minus {
                self.consume(); // consume minus
                if let Some(maybe_greather_then) = self.consume() {
                    if maybe_greather_then.token_type == TokenType::GreaterThen {
                        if let Some(token) = self.consume() {
                            match token.token_type {
                                TokenType::Identifier(id) => {
                                    return_type = UnresolvedType { name: id };
                                }
                                _ => {}
                            }
                        }
                    } else {
                        self.error(
                            "Unexpected ending, expected '>' after '-' to indicate return type",
                        );
                    }
                } else {
                    self.error("Unexpected ending, expected '>' after '-' to indicate return type");
                }
            }
        } else {
            self.error("Unexpected ending at function declaration, expected either -> or left curly brackets");
        }

        let implementation = self.bblock();
        assert!(implementation.is_some());
        return Some(Ast::FunDecl {
            name: "test_func".to_owned(),
            args,
            returns: return_type,
            implementation,
        });
    }

    fn struct_decl(&mut self) -> Option<Ast> {
        None
    }

    fn call(&mut self) -> Option<ExprKind> {
        if let Some(callee_name_token) = self.consume() {
            let mut callee = String::from("unknown callee");
            if let TokenType::Identifier(id) = callee_name_token.token_type {
                callee = id;
            } else {
                self.error("Expected identifier as callee name");
            }
            if let Some(_left_paren_token) = self.consume() {
                let mut args: Vec<ExprKind> = vec![];
                loop {
                    if let Some(arg) = self.expr(None) {
                        args.push(arg);
                    } else {
                        break;
                    }

                    if let None = self.consume_tokentype(TokenType::Comma) {
                        // No more arguments, so break the loop.
                        break;
                    }
                }

                if let None = self.consume_tokentype(TokenType::RightParen) {
                    self.error("Expected closing parent of function arguments");
                }

                return Some(ExprKind::CallExpr {
                    callee: callee,
                    args,
                });
            }
        }

        None
    }

    pub fn parse(&mut self) -> Vec<Ast> {
        let mut ast: Vec<Ast> = vec![];
        loop {
            if let Some(t) = self.peek() {
                match t.token_type.borrow() {
                    // Top level can only be a struct or a function at this point.
                    TokenType::Func => {
                        if let Some(statement) = self.func_decl() {
                            ast.push(statement);
                        } else {
                            self.error("Failed to parse function declaration");
                        }
                    }
                    TokenType::Struct => {
                        if let Some(struct_def) = self.struct_decl() {
                            ast.push(struct_def);
                        } else {
                            self.error("Failed to parse struct declaration");
                        }
                    }
                    _ => {
                        // unknown symbol
                        panic!("Unknown symbol at top level, got {:?}", t);
                    }
                };
            } else {
                // No more tokens to parse
                break;
            }
        }

        return ast;
    }
}

#[test]
fn test_expressions() {
    let two = Rc::new(ExprKind::Literal(LiteralExpr {
        constant_type: BuildinTypeKind::Int,
        value: 2,
    }));
    let three = Rc::new(ExprKind::Literal(LiteralExpr {
        constant_type: BuildinTypeKind::Int,
        value: 3,
    }));
    let four = Rc::new(ExprKind::Literal(LiteralExpr {
        constant_type: BuildinTypeKind::Int,
        value: 4,
    }));
    let three_time_four = Rc::new(ExprKind::Binary(BinaryExpr {
        operator: BinaryOperatorKind::Mul,
        left: three.clone(),
        right: four.clone(),
    }));

    let first_expr_ast = Rc::new(ExprKind::Binary(BinaryExpr {
        operator: BinaryOperatorKind::Plus,
        left: two.clone(),
        right: three_time_four,
    }));

    let second_expr_ast = Rc::new(ExprKind::Binary(BinaryExpr {
        operator: BinaryOperatorKind::Plus,
        left: Rc::new(ExprKind::Binary(BinaryExpr {
            operator: BinaryOperatorKind::Mul,
            left: two.clone(),
            right: three.clone(),
        })),
        right: four,
    }));

    let test_data = vec![("2+3*4", first_expr_ast), ("2*3+4", second_expr_ast)];

    for (str_expression, expected_parsed_expression) in test_data {
        let lexer_res =
            lexer::Lexer::new(std::path::PathBuf::new(), str_expression.to_owned()).scan();
        let expr = Parser::new(lexer_res.tokens).expr(None);

        assert!(expr.is_some());
        let expr = expr.unwrap();
        let l = expr.borrow();
        let r = expected_parsed_expression.borrow();
        assert_eq!(l, r);
    }
}

#[test]
fn test_ast() {
    let str_expression = "func square(a: int) -> int {
            return a;
        }";
    let return_statement = Ast::ReturnStatement(Some(ExprKind::Binary(BinaryExpr {
        operator: BinaryOperatorKind::Mul,
        left: Rc::new(ExprKind::Identifier("a".to_owned())),
        right: Rc::new(ExprKind::Literal(LiteralExpr {
            constant_type: BuildinTypeKind::Int,
            value: 2,
        })),
    })));
    let fun_arg = FunArg {
        name: "a".to_owned(),
        arg_type: UnresolvedType {
            name: "int".to_owned(),
        },
    };
    let expected_expression = Ast::FunDecl {
        name: "func_square".to_string(),
        args: vec![fun_arg],
        returns: UnresolvedType {
            name: "int".to_owned(),
        },
        implementation: Some(Box::new(Ast::BasicBlock {
            statements: vec![Box::new(return_statement)],
        })),
    };

    let lexer_res = lexer::Lexer::new(std::path::PathBuf::new(), str_expression.to_owned()).scan();
    println!("lexer_res: {:?}", lexer_res);
    let mut parser = Parser::new(lexer_res.tokens);
    let expr = parser.parse();

    assert!(!expr.is_empty());
    if let Some(res_func_def_expr) = expr.first() {
        println!("parsed expression {:?}", res_func_def_expr);
        assert_eq!(res_func_def_expr, &expected_expression);
    } else {
        panic!("Expected function definition");
    }
}
