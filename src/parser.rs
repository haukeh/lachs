use std::iter::{self, Peekable};

use crate::{
    ast::Expr,
    scanner::{LiteralValue, Token, TokenType},
};

struct TokenIter {
    tokens: Vec<Token>,
}

impl Iterator for TokenIter {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.len() > 0 {
            return Some(self.tokens.remove(0));
        }
        None
    }
}

pub struct Parser {
    tokens: Peekable<TokenIter>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: TokenIter { tokens }.peekable(),
        }
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while let Some(op) = self.matches_one(vec![TokenType::Bang, TokenType::BangEqual]) {
            let right = Box::new(self.comparison());
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            }
        }
        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        while let Some(op) = self.matches_one(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let right = Box::new(self.term());
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            }
        }
        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        while let Some(op) = self.matches_one(vec![TokenType::Minus, TokenType::Plus]) {
            let right = Box::new(self.factor());
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            }
        }
        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();
        while let Some(op) = self.matches_one(vec![TokenType::Slash, TokenType::Star]) {
            let right = Box::new(self.unary());
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            }
        }
        expr
    }

    fn unary(&mut self) -> Expr {
        if let Some(op) = self.matches_one(vec![TokenType::Bang, TokenType::Minus]) {
            let right = Box::new(self.unary());
            return Expr::Unary { op, right };
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        if let Some(_) = self.matches(TokenType::False) {
            return Expr::Literal(LiteralValue::False);
        }
        if let Some(_) = self.matches(TokenType::True) {
            return Expr::Literal(LiteralValue::True);
        }
        if let Some(_) = self.matches(TokenType::Nil) {
            return Expr::Literal(LiteralValue::Nil);
        }
        if let Some(tok) = self.matches_one(vec![TokenType::Number, TokenType::String]) {
            return Expr::Literal(tok.literal.expect("expected Number or String literal"));
        }
        if let Some(_) = self.matches(TokenType::LeftParen) {
            let expr = self.expression();
            self.consume(TokenType::RightParen, "Expect ')' after expression.");
            return Expr::Grouping {
                expr: Box::new(expr),
            };
        }

        panic!("todo: errors");
    }

    fn consume(&mut self, tt: TokenType, err: &str) {
        if self.peek_for(tt) {
            let s = self.tokens.next();
        }
        panic!("todo: errors {}", err);
    }

    fn matches(&mut self, t: TokenType) -> Option<Token> {
        self.matches_one(iter::once(t))
    }

    fn matches_one(&mut self, types: impl IntoIterator<Item = TokenType>) -> Option<Token> {
        self.tokens
            .next_if(|t| types.into_iter().any(|tt| t.typ == tt))
    }

    fn peek_for(&mut self, tt: TokenType) -> bool {
        self.peek().filter(|&t| t.typ == tt).is_some()
    }

    fn at_end(&mut self) -> bool {
        self.peek_for(TokenType::Eof)
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn synchronize(&mut self) {
        while let Some(tok) = self.tokens.next() {
            if tok.typ == TokenType::Semicolon {
                return;
            }
            if let Some(next) = self.peek() {
                match next.typ {
                    TokenType::Class
                    | TokenType::Fun
                    | TokenType::Var
                    | TokenType::For
                    | TokenType::If
                    | TokenType::While
                    | TokenType::Print
                    | TokenType::Return => {
                        return;
                    }
                    _ => { // ignore}
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        let mut p = Parser::new(vec![
            Token::new(TokenType::Comma, ",".to_string(), None, 1),
            Token::new(TokenType::Dot, ".".to_string(), None, 1),
        ]);
    }
}
