pub mod ast;

use ast::*;
use lexer::token::{Associativity, Location, Token, TokenType};
use std::borrow::Borrow;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(String, Location),
    MissingToken(String, Location),
    InvalidExpression(String, Location),
    UnexpectedEndOf(String),
    NotImplemented(String),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::UnexpectedToken(msg, loc) => write!(f, "Unexpected token: {msg} at {:?}", loc),
            ParserError::MissingToken(msg, loc) => write!(f, "Missing token: {msg} at {:?}", loc),
            ParserError::InvalidExpression(msg, loc) => write!(f, "Invalid expression: {msg} at {:?}", loc),
            ParserError::UnexpectedEndOf(msg) => write!(f, "Unexpected end of file: {msg}"),
            ParserError::NotImplemented(msg) => write!(f, "Not implemented: {msg}"),
        }
    }
}

impl std::error::Error for ParserError {}

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
        return Some(self.tokens[self.current - 1].clone())
    }

    // Not sure if this is a good idea.
    pub fn undo_consume(&mut self) -> bool {
        if self.current <= 0 {
            return false;
        }
        self.current = self.current - 1;
        return true;
    }

    fn try_consume_tokentype(&mut self, tok_type: TokenType) -> Result<Token, ParserError> {
        // This is sometimes problematic as TokenType can contain values
        // in which case we can't provide it as an argument here as we don't
        // know the value of it's values.
        if let Some(tok) = self.peek() {
            if tok.token_type == tok_type {
                return Ok(self.consume().unwrap());
            }

            return Err(ParserError::UnexpectedToken(format!(
                "Expected token type: {:?} but got {:?}",
                tok_type,
                &tok.token_type
            ), tok.location.clone()))
        }
        return Err(ParserError::UnexpectedEndOf(format!(
            "Expected token type: {:?}",
            tok_type
        )))

    }

    fn try_identifier(&mut self) -> Result<String, ParserError> {
        if let Some(token) = self.consume() {
            if let TokenType::Identifier(id) = token.token_type {
                return Ok(id);
            }
            self.undo_consume();
            return Err(ParserError::UnexpectedToken(format!(
                "Expected identifier but got token {:?}",
                token
            ), token.location.clone()));
        }
        self.undo_consume();
        return Err(ParserError::UnexpectedEndOf(
            "Expected identifier".to_string(),
        ));
    }

    fn try_number(&mut self) -> Result<LiteralExpr, ParserError> {
        if let Some(t) = self.consume() {
            return match t.token_type {
                TokenType::Number(n) => Ok(LiteralExpr {
                    constant_type: BuildinTypeKind::Int,
                    value: n,
                }),
                _ => {
                    self.undo_consume();
                    Err(ParserError::UnexpectedToken(format!(
                        "Expected a number but got token {:?}",
                        t
                    ), t.location.clone()))
                }
            };
        }
        self.undo_consume();
        Err(ParserError::UnexpectedEndOf(
            "No token consumed".to_string(),
        ))
    }

    fn number_or_identifier(&mut self) -> Result<ExprKind, ParserError> {
        if let Ok(id) = self.try_identifier() {
            return Ok(ExprKind::Identifier(id));
        } else if let Ok(number) = self.try_number() {
            return Ok(ExprKind::Literal(number));
        } else {
            if let Some(token) = self.peek() {
                return Err(ParserError::UnexpectedToken(
                    "Expected identifier or number".to_string(),
                    token.location.clone(),
                ));
            }
            return Err(ParserError::UnexpectedEndOf(
                "Expected identifier or number".to_string(),
            ));
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

    fn build_operator(
        operator_token: Token,
        lhs: ExprKind,
        rhs: ExprKind,
    ) -> Result<ExprKind, ParserError> {
        match operator_token.token_type {
            t if t.is_binary_operator() => Ok(ExprKind::Binary(BinaryExpr {
                operator: Parser::token_type_to_binary_op(t).unwrap(),
                left: Rc::new(lhs),
                right: Rc::new(rhs),
            })),
            _ => Err(ParserError::UnexpectedToken(format!(
                "Expected a binary operator but got token {:?}",
                operator_token
            ), operator_token.location.clone())),
        }
    }

    fn expr(&mut self, current_precendence: Option<i32>) -> Result<ExprKind, ParserError> {
        // higher precendence is more sticky...
        let current_precendence = current_precendence.unwrap_or(-1);

        // https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
        // We should alway be able to get the identifier/number if this is an expression.
        let mut lhs = self.number_or_identifier()?;

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
                    return Ok(lhs); // exit the expression evaluation
                }
                _other => {
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

                let rhs = self.expr(Some(next_precendence))?;
                lhs = Parser::build_operator(operator_token, lhs, rhs)?;
            } else {
                // if no more binary operators -> exit
                return Ok(lhs);
            }
        }

        Ok(lhs)
    }

    fn expr_stmt(&mut self) -> Result<Ast, ParserError> {
        let lhs = self.expr(None)?;
        if let Some(seperator) = self.peek() {
            match seperator.token_type {
                TokenType::Assign => {
                    self.consume();
                    let rhs = self.expr(None)?;
                    return Ok(Ast::Assign(AssignExpr {
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    }));
                }
                _ => {
                    self.try_consume_tokentype(TokenType::SemiColon)?;
                }
            }
        }

        return Err(ParserError::UnexpectedEndOf(
            "expression statement".to_owned(),
        ));
    }

    fn if_stmt(&mut self) -> Result<Ast, ParserError> {
        return Err(ParserError::NotImplemented(
            "if statement not implemented".to_owned(),
        ));
    }

    fn return_stmt(&mut self) -> Result<Ast, ParserError> {
        self.try_consume_tokentype(TokenType::Return)?;
        if self.try_consume_tokentype(TokenType::SemiColon).is_ok() {
            // no expression after return;
            return Ok(Ast::ReturnStatement(None));
        }
        let expr = self.expr(None)?;
        self.try_consume_tokentype(TokenType::SemiColon)?;
        Ok(Ast::ReturnStatement(Some(expr)))
    }

    fn stmt(&mut self) -> Result<Ast, ParserError> {
        if let Some(t) = self.peek() {
            return match &t.token_type {
                TokenType::If => self.if_stmt(),
                TokenType::Return => self.return_stmt(),
                _ => return self.expr_stmt(),
            };
        }
        return Err(ParserError::UnexpectedEndOf("statement".to_owned()));
    }

    fn bblock(&mut self) -> Result<Box<Ast>, ParserError> {
        if let Some(t) = self.consume() {
            match t.token_type {
                TokenType::LeftCurlyBrackets => {}
                TokenType::Var => {
                    let var_decl = self.var_decl()?;
                    return Ok(Box::new(var_decl));
                }
                _ => {
                    return Err(ParserError::UnexpectedEndOf(
                        "basic block -> missing open brackets on block".to_owned(),
                    ));
                },
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
                    _ => Some(self.stmt()?),
                };

                if let Some(stmt) = maybe_statement {
                    statements.push(Box::new(stmt));
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return Ok(Box::new(Ast::BasicBlock { statements }));
    }

    fn var_decl(&mut self) -> Result<Ast, ParserError> {
        self.try_consume_tokentype(TokenType::Var)?;
        let name = self.try_identifier()?;
        let rhs = self.expr(None)?;
        return Ok(Ast::VarDecl { name, rhs });
    }

    fn func_decl(&mut self) -> Result<Ast, ParserError> {
        self.try_consume_tokentype(TokenType::Func {})?;

        let name = self.try_identifier()?;
        self.try_consume_tokentype(TokenType::LeftParen {})?;

        let mut args: Vec<FunArg> = vec![];
        loop {
            let name = self.try_identifier()?;
            self.try_consume_tokentype(TokenType::Colon {})?;

            let type_name = self.try_identifier()?;
            let arg_type = UnresolvedType { name: type_name };
            args.push(FunArg { name, arg_type });

            if self.try_consume_tokentype(TokenType::Comma).is_err() {
                // No more arguments, so break the loop.
                break;
            }

            if self
                .peek()
                .map_or(false, |t| t.token_type == TokenType::RightParen)
            {
                break;
            }
        }
        self.try_consume_tokentype(TokenType::RightParen)?;

        // assume no return type by default
        let mut return_type = UnresolvedType {
            name: "void".to_owned(),
        };
        // Try to consume arrow or left curly brackets
        if self.try_consume_tokentype(TokenType::LeftCurlyBrackets).is_err() {
            self.try_consume_tokentype(TokenType::Arrow)?;
            let id = self.try_identifier()?;
            return_type = UnresolvedType { name: id };
        }


        let implementation = self.bblock()?;
        return Ok(Ast::FunDecl {
            name: name.to_owned(),
            args,
            returns: return_type,
            implementation: Some(implementation),
        });
    }

    fn struct_decl(&mut self) -> Result<Ast, ParserError> {
        return Err(ParserError::NotImplemented(
            "struct declaration not implemented".to_owned(),
        ));
    }

    fn call(&mut self) -> Result<ExprKind, ParserError> {
        let callee = self.try_identifier()?;
        if let Some(_left_paren_token) = self.consume() {
            let mut args: Vec<ExprKind> = vec![];
            loop {
                let arg = self.expr(None)?;
                args.push(arg);

                if self.try_consume_tokentype(TokenType::Comma).is_ok() {
                    // No more arguments, so break the loop.
                    break;
                }
            }

            return Ok(ExprKind::CallExpr {
                callee: callee,
                args,
            });
        }

        return Err(ParserError::UnexpectedEndOf(
            "failed to parse function call".to_owned(),
        ));
    }

    pub fn parse(&mut self) -> Result<Vec<Ast>, ParserError> {
        let mut ast: Vec<Ast> = vec![];
        loop {
            if let Some(t) = self.peek() {
                match t.token_type {
                    // Top level can only be a struct or a function at this point.
                    TokenType::Func => {
                        ast.push(self.func_decl()?);
                    }
                    TokenType::Struct => {
                        ast.push(self.struct_decl()?);
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

        return Ok(ast);
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

        if let Err(e) = expr {
            panic!("Error parsing expression: {e}");
        }

        assert!(expr.is_ok());
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
    if let Err(e) = expr {
        panic!("Error parsing function: {e}");
    }
    let expr = expr.unwrap();
    println!("Parsed expression: {:?}", expr);
    assert_eq!(expr.len(), 1, "Expected exactly one function definition");

    if let Some(res_func_def_expr) = expr.first() {
        println!("parsed expression {:?}", res_func_def_expr);
        assert_eq!(res_func_def_expr, &expected_expression);
    } else {
        panic!("Expected function definition");
    }
}
