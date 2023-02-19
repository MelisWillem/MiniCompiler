pub mod token;

use std::path::PathBuf;
use token::{*};

pub struct Lexer {
    file_path: PathBuf,
    characters: Vec<char>,
    current: usize, // Cursor in the characters vector of the current position that we want to parse.
    begin_token: usize, // When starting to parse the token, this will be equal to current.
    line_number: usize, // Usefull to create nice errors.
    column: usize,  // Usefile to create nice errors.
    errors: Vec<String>,
}

#[derive(Debug)]
pub struct LexerResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<String>,
}

impl Lexer {
    pub fn new(file_path: PathBuf, file_as_string: String) -> Lexer {
        Lexer {
            file_path,
            characters: file_as_string.chars().collect(),
            current: 0,
            begin_token: 0,
            line_number: 0,
            column: 0,
            errors: Vec::new(),
        }
    }

    fn reset(&mut self) {
        self.current = 0;
        self.begin_token = 0;
        self.line_number = 0;
        self.column = 0
    }

    fn at_end(&self) -> bool {
        self.current >= self.characters.len()
    }

    fn peek(&self) -> Option<char> {
        if self.at_end() {
            return None;
        }

        Some(self.characters[self.current])
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.peek();
        self.current = self.current + 1;
        self.column = self.column + 1;

        let is_end_of_line = c.map_or(false, |c| c == '\n');
        if is_end_of_line {
            self.line_number = self.line_number + 1;
            self.column = 0;
        }

        c
    }

    pub fn create_token(&mut self, token_type: TokenType) -> Token {
        self.begin_token = self.current;

        Token::new(
            token_type,
            self.file_path.clone(),
            self.line_number,
            self.column,
        )
    }

    fn error(&mut self, message: &str) {
        // TODO: Don't panic but store the error in a nice format so we can return good errors
        let error = format!(
            "Parser stopped at column={} line={} begin of toke={} with error {}",
            self.column, self.line_number, self.begin_token, message
        );

        self.errors.push(error);
    }

    fn consume_while<F>(&mut self, predicate: F)
    where
        F: Fn(char) -> bool,
    {
        loop {
            if let Some(v) = self.peek() {
                if predicate(v) {
                    self.consume();
                } else {
                    // Bad char do not consome and quit.
                    break;
                }
            } else {
                // No more chars left.
                break;
            }
        }
    }

    fn current_token_strings(&self) -> String {
        self.characters[self.begin_token..self.current]
            .iter()
            .collect()
    }

    fn consume_number(&mut self) -> Option<i32> {
        self.consume_while(|c| c.is_numeric());
        if let Some(c) = self.peek() {
            if c.is_alphabetic() {
                self.error("Can't have alphabetic char after number.");
                // What should we do to recover?
                // Maybe we should consume to the end of the word and spit that out as error?
            }
        }

        let parse_result = self.current_token_strings().parse::<i32>();

        match parse_result {
            Ok(i) => Some(i),
            Err(e) => {
                let error_message = format!("Enable to parsing number, with error {e}");
                self.error(&error_message);
                // What should we do to recover?
                None // After reporting the error return none.
            }
        }
    }

    fn consume_word(&mut self) -> Option<String> {
        self.consume_while(|c| c.is_numeric() || c.is_alphabetic() || c == '_');

        let total_string = self.current_token_strings();
        match total_string.len() {
            0 => None,
            _ => Some(total_string)
        }
    }

    pub fn scan(&mut self) -> LexerResult {
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            if let Some(n) = self.consume() {
                match n{
                   '='  => {
                        if self.peek().map_or(false, |c| c == '='){
                            self.consume(); // consume the second '='
                            tokens.push(self.create_token(TokenType::Equal))
                        }
                        else{
                            println!("bingo2");
                            tokens.push(self.create_token(TokenType::Assign))
                        }
                    },
                   '+' =>  {tokens.push(self.create_token(TokenType::Plus))},
                   '-'  => {tokens.push(self.create_token(TokenType::Minus))},
                   '*'  => {tokens.push(self.create_token(TokenType::Mul))},
                   '/'  => {tokens.push(self.create_token(TokenType::Div))},
                   ' '  => {tokens.push(self.create_token(TokenType::Space))},
                   '\n'  => {tokens.push(self.create_token(TokenType::EndOfLine))},
                   '('  => {tokens.push(self.create_token(TokenType::LeftParen))},
                   ')'  => {tokens.push(self.create_token(TokenType::RightParen))},
                   '['  => {tokens.push(self.create_token(TokenType::LeftSquareBrackets))},
                   ']'  => {tokens.push(self.create_token(TokenType::RightSquareBrackets))},
                   '{'  => {tokens.push(self.create_token(TokenType::LeftCurlyBrackets))},
                   '}'  => {tokens.push(self.create_token(TokenType::RightCurlyBrackets))},
                   c => {
                    if c.is_numeric()
                    {
                        if let Some(number) = self.consume_number()
                        {
                            tokens.push(self.create_token(TokenType::Number(number)));
                        }
                        else {
                            // Failed at reading the number
                            // Try to recover and continue reading
                        }
                    }
                    else if c.is_alphabetic()
                    {
                        if let Some(word) = self.consume_word()
                        {
                            match word.as_str() {
                                "func"  => tokens.push(self.create_token(TokenType::Func)),
                                "if"  => tokens.push(self.create_token(TokenType::If)),
                                "else"  => tokens.push(self.create_token(TokenType::Else)),
                                "var"  => tokens.push(self.create_token(TokenType::Var)),
                                _ => tokens.push(self.create_token(TokenType::Word(word)))
                            }
                        }
                        else{
                            // Failed at reading word
                            // Try to recover and continue reading
                        }
                    }
                    else{
                        self.error(format!("Unknown symbol {c}").as_str());
                        break;
                    }
                   }
                   // TokenType::to_string(TokenType::Func)  => TokenType::Func,
                   // TokenType::to_string(TokenType::Struct)  => TokenType::Struct,
                };
            } else {
                break;
            }
        }

        let res = LexerResult {
            tokens: tokens,
            errors: self.errors.clone(),
        };

        self.reset(); // In case we want to run it again.

        res
    }
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_char_tokens() {
        let input = "+-*/-=== (){}[]";
        let expected_types = vec![
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Mul,
            TokenType::Div,
            TokenType::Minus,
            TokenType::Equal,
            TokenType::Assign,
            TokenType::Space,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftCurlyBrackets,
            TokenType::RightCurlyBrackets,
            TokenType::LeftSquareBrackets,
            TokenType::RightSquareBrackets
        ];

        let mut sut = Lexer::new(PathBuf::new(), String::from(input));
        let res = sut.scan();

        let res_types: Vec<TokenType> = res.tokens.into_iter().map(|t| t.token_type).collect();

        assert_eq!(expected_types, res_types);
    }

    #[test]
    fn keywords() {
        let input = "if else var func";
        let expected_types = vec![
            TokenType::If,
            TokenType::Else,
            TokenType::Var,
            TokenType::Func,
        ];

        let mut sut = Lexer::new(PathBuf::new(), String::from(input));
        let res = sut.scan();

        let res_types: Vec<TokenType> = res
            .tokens
            .into_iter()
            .map(|t| t.token_type)
            .filter(|t| t != &TokenType::Space) // ignore the spaces
            .collect();

        println!("{:?}", res.errors);

        assert_eq!(expected_types, res_types);
    }
}