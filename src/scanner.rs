use std::fmt::Display;

use phf::phf_map;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals,
    Identifier,
    String,
    Number,

    // Keywords,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Number(f64),
    String(String),
    True,
    False,
    Nil
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::Number(n) => write!(f, "{}", n),
            LiteralValue::String(s) => write!(f, "{}", s),
            LiteralValue::True => write!(f, "true"),
            LiteralValue::False => write!(f, "false"),
            LiteralValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub lexeme: String,
    pub literal: Option<LiteralValue>,
    line: usize,
}

impl Token {
    pub fn new(tt: TokenType, lexeme: String, literal: Option<LiteralValue>, line: usize) -> Self {
        Token {
            typ: tt,
            lexeme,
            literal,
            line,
        }
    }
}

static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "and" => TokenType::And,
    "class" => TokenType::Class,
    "else" => TokenType::Else,
    "false" => TokenType::False,
    "fun" => TokenType::Fun,
    "for" => TokenType::For,
    "if" => TokenType::If,
    "nil" => TokenType::Nil,
    "or" => TokenType::Or,
    "print" => TokenType::Print,
    "return" => TokenType::Return,
    "super" => TokenType::Super,
    "this" => TokenType::This,
    "true" => TokenType::True,
    "var" => TokenType::Var,
    "while" => TokenType::While,
};

pub struct Scanner {
    source: Vec<u8>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            source: source.into_bytes(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 0,
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token {
            typ: TokenType::Eof,
            lexeme: "".to_string(),
            literal: None,
            line: self.line,
        });

        return &self.tokens;
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> u8 {
        let next = self.source[self.current];
        self.current += 1;
        next
    }

    fn matches(&mut self, expected: u8) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source[self.current] != expected {
            return false;
        }

        self.current += 1;

        return true;
    }

    fn add_token(&mut self, tt: TokenType) {
        self.add_token_literal(tt, None);
    }

    fn add_conditional_token(&mut self, expected: u8, then: TokenType, otherwise: TokenType) {
        let token = if self.matches(expected) {
            then
        } else {
            otherwise
        };
        self.add_token(token)
    }

    fn add_token_literal(&mut self, tt: TokenType, literal: Option<LiteralValue>) {
        let lexeme = &self.source[self.start..self.current];
        let lexeme = String::from_utf8_lossy(lexeme).to_string();

        self.tokens.push(Token {
            typ: tt,
            lexeme,
            literal,
            line: self.line,
        });
    }

    fn peek(&self) -> u8 {
        if self.is_at_end() {
            return b'\0';
        }
        return self.source[self.current];
    }

    fn peek_next(&self) -> u8 {
        if self.current + 1 > self.source.len() {
            return b'\0';
        }
        return self.source[self.current + 1];
    }

    fn string(&mut self) {
        while self.peek() != b'"' && !self.is_at_end() {
            if self.peek() == b'\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            // TODO: error
        }

        self.advance();

        let literal = &self.source[self.start + 1..self.current - 1];
        let string = String::from_utf8_lossy(literal).to_string();
        self.add_token_literal(TokenType::String, Some(LiteralValue::String(string)))
    }

    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        let s = String::from_utf8_lossy(&self.source[self.start..self.current]);
        match s.parse::<f64>() {
            Ok(num) => {
                self.add_token_literal(TokenType::Number, Some(LiteralValue::Number(num)));
            }
            Err(_) => todo!("error"),
        }
    }

    fn identifier(&mut self) {
        while Scanner::is_valid_character(self.peek()) {
            self.advance();
        }
        let text = String::from_utf8_lossy(&self.source[self.start..self.current]);

        let token_type = KEYWORDS
            .get(text.as_ref())
            .cloned()
            .unwrap_or(TokenType::Identifier);

        self.add_token(token_type);
    }

    fn is_valid_character(c: u8) -> bool {
        c.is_ascii_alphabetic() || c == b'_'
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            b'(' => self.add_token(TokenType::LeftParen),
            b')' => self.add_token(TokenType::RightParen),
            b'{' => self.add_token(TokenType::LeftBrace),
            b'}' => self.add_token(TokenType::RightBrace),
            b',' => self.add_token(TokenType::Comma),
            b'.' => self.add_token(TokenType::Dot),
            b'-' => self.add_token(TokenType::Minus),
            b'+' => self.add_token(TokenType::Plus),
            b';' => self.add_token(TokenType::Semicolon),
            b'*' => self.add_token(TokenType::Star),
            b'!' => self.add_conditional_token(b'=', TokenType::BangEqual, TokenType::Bang),
            b'=' => self.add_conditional_token(b'=', TokenType::EqualEqual, TokenType::Equal),
            b'<' => self.add_conditional_token(b'=', TokenType::LessEqual, TokenType::Less),
            b'>' => self.add_conditional_token(b'=', TokenType::GreaterEqual, TokenType::Greater),
            b'/' => {
                if self.matches(b'/') {
                    while self.peek() != b'\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            b' ' | b'\r' | b'\t' => {}
            b'\n' => self.line += 1,
            b'"' => self.string(),
            _ => {
                if c.is_ascii_digit() {
                    self.number();
                } else if Scanner::is_valid_character(c) {
                    self.identifier();
                } else {
                    todo!("error impl");
                }
            }
        }
    }
}
