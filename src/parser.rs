use std::{iter::{self, Peekable}, os::macos::raw::stat};
use thiserror::Error;

use crate::{
    ast::{Expr, Stmt},
    scanner::{LiteralValue, Token, TokenType},
};

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected Token {0:?}")]
    UnexpectedToken(Token),
    #[error("Unexpected end of token stream")]
    UnexpectedEOF,
    #[error("Expected token not found: {0}")]
    ExpectedTokenNotFound(String)
}

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
    tokens: Peekable<TokenIter>
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: TokenIter { tokens }.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = Vec::new();
        while !self.at_end() {
            let s = self.statement()?;
            stmts.push(s)
        }
        Ok(stmts)
    }

    fn declaration(&mut self) -> Result<Stmt, ParserError> { 
        let res = if let Some(_) = self.matches(TokenType::Var) {
            self.var_decl()
        } else {
            self.statement()
        };
        match res {
            ok @ Ok(_) => ok,
            err @ Err(_) => {
                self.synchronize();
                err
            },
        }
    }

    fn var_decl(&mut self) -> Result<Stmt, ParserError> {
        let name = self.consume(TokenType::Identifier, "Expected variable name.")?;
        let mut initializer: Expr = Expr::Literal(LiteralValue::Nil);
        if let Some(_) = self.matches(TokenType::Equal) {
            initializer = self.expression()?;
        }
        self.consume(TokenType::Semicolon, "Expected ';' after variable declaration.")?;
        
        Ok(Stmt::Var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if let Some(_) = self.matches(TokenType::Print) {
            return self.print_stmt()
        } 
        return self.expression_stmt()        
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParserError> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ';' after value.")?;
        Ok(Stmt::Print(value))
    }

    fn expression_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ';' after expression.")?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while let Some(op) = self.matches_one(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let right = Box::new(self.comparison()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            }
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;
        while let Some(op) = self.matches_one(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let right = Box::new(self.term()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            }
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;
        while let Some(op) = self.matches_one(vec![TokenType::Minus, TokenType::Plus]) {
            let right = Box::new(self.factor()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            }
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while let Some(op) = self.matches_one(vec![TokenType::Slash, TokenType::Star]) {
            let right = Box::new(self.unary()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(op) = self.matches_one(vec![TokenType::Bang, TokenType::Minus]) {
            let right = Box::new(self.unary()?);
            return Ok(Expr::Unary { op, right });
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if let Some(_) = self.matches(TokenType::False) {
            return Ok(Expr::Literal(LiteralValue::False))
        }
        if let Some(_) = self.matches(TokenType::True) {
            return Ok(Expr::Literal(LiteralValue::True))
        }
        if let Some(_) = self.matches(TokenType::Nil) {
            return Ok(Expr::Literal(LiteralValue::Nil))
        }
        if let Some(tok) = self.matches_one(vec![TokenType::Number, TokenType::String]) {
            return Ok(Expr::Literal(
                tok.literal.expect("expected Number or String literal"),
            ))
        }
        if let Some(identifier) = self.matches(TokenType::Identifier) {
            return Ok(Expr::Variable(identifier))
        }
        if let Some(_) = self.matches(TokenType::LeftParen) {
            let expr = self.expression()?;
            let _ = self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping {
                expr: Box::new(expr),
            });
        }

        match self.tokens.next() {
            Some(tok) => Err(ParserError::UnexpectedToken(tok)),
            None => Err(ParserError::UnexpectedEOF),
        }
    }

    fn consume(&mut self, tt: TokenType, err: &str) -> Result<Token, ParserError> {
        if self.peek_for(tt) {
            Ok(self.tokens.next().unwrap())
        } else {
            Err(ParserError::ExpectedTokenNotFound(err.to_string()) )
        } 
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
                    _ => {} // ignore
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::AstPrinter;

    use super::*;

    #[test]
    fn test_parser() {
        let mut p = Parser::new(vec![
            Token::new(TokenType::Number, "14".to_string(), Some(LiteralValue::Number(14f64)), 1),
            Token::new(TokenType::Plus, "+".to_string(), None, 1),
            Token::new(TokenType::Number, "6".to_string(), Some(LiteralValue::Number(6f64)), 1),
        ]);

        let res = p.parse().unwrap();
        
        assert_eq!("(+ 14 6)", AstPrinter::new().string(&res));
    }
}
