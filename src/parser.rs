use std::iter::{self, Peekable};
use thiserror::Error;

use crate::{
    ast::{Expr, FunctionDecl, Stmt},
    scanner::{LiteralValue, Token, TokenType},
};

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected Token {0:?}")]
    UnexpectedToken(Token),
    #[error("Unexpected end of token stream")]
    UnexpectedEOF,
    #[error("Expected token not found: {0}")]
    ExpectedTokenNotFound(String),
    #[error("Function argument limit of 255 was exceeded")]
    TooManyFnArguments,
}

#[derive(Debug)]
enum FunctionKind {
    Func,
    _Method,
}

struct TokenIter {
    tokens: Vec<Token>,
}

impl Iterator for TokenIter {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.tokens.is_empty() {
            return Some(self.tokens.remove(0));
        }
        None
    }
}

pub struct Parser {
    tokens: Peekable<TokenIter>,
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
            let s = self.declaration()?;
            stmts.push(s)
        }
        Ok(stmts)
    }

    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        let res = if self.take_if_matches(TokenType::Var).is_some() {
            self.var_decl()
        } else if self.take_if_matches(TokenType::Fun).is_some() {
            self.function(FunctionKind::Func)
        } else {
            self.statement()
        };
        
        if res.is_err() {
            self.synchronize();
        }
        
        res        
    }

    fn function(&mut self, kind: FunctionKind) -> Result<Stmt, ParserError> {
        let name = self.must_consume(TokenType::Identifier, &format!("Expected {:?} name.", kind))?;
        self.must_consume(
            TokenType::LeftParen,
            &format!("Expected '(' after {:?} name.", kind),
        )?;
        let mut params = Vec::new();
        if !self.next_is(TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(ParserError::TooManyFnArguments);
                }

                params.push(self.must_consume(TokenType::Identifier, "Expected parameter name.")?);

                if self.take_if_matches(TokenType::Comma).is_none() {
                    break;
                }
            }
        }
        self.must_consume(TokenType::RightParen, "Expected ')' after parameters.")?;
        self.must_consume(
            TokenType::LeftBrace,
            &format!("Expected '{{' before {:?} body.", kind),
        )?;

        let body = self.block()?;

        Ok(Stmt::Function(FunctionDecl { name, params, body }))
    }

    fn var_decl(&mut self) -> Result<Stmt, ParserError> {
        let name = self.must_consume(TokenType::Identifier, "Expected variable name.")?;
        let mut initializer = None;
        if self.take_if_matches(TokenType::Equal).is_some() {
            initializer = Some(self.expression()?);
        }
        self.must_consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        )?;

        Ok(Stmt::Var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.take_if_matches(TokenType::For).is_some() {
            return self.for_statement();
        }
        if self.take_if_matches(TokenType::If).is_some() {
            return self.if_statement();
        }
        if self.take_if_matches(TokenType::Print).is_some() {
            return self.print_stmt();
        }
        if let Some(tok) = self.take_if_matches(TokenType::Return) {
            return self.return_statement(tok);
        }
        if self.take_if_matches(TokenType::While).is_some() {
            return self.while_statement();
        }
        if self.take_if_matches(TokenType::LeftBrace).is_some() {
            return Ok(Stmt::Block(self.block()?));
        }
        self.expression_stmt()
    }

    fn return_statement(&mut self, keyword: Token) -> Result<Stmt, ParserError> {
        let mut value = Expr::Literal(LiteralValue::Nil);

        if !self.next_is(TokenType::Semicolon) {
            value = self.expression()?;
        }

        self.must_consume(TokenType::Semicolon, "Expected ';' after return value")?;

        Ok(Stmt::Return(keyword, Box::new(value)))
    }

    fn for_statement(&mut self) -> Result<Stmt, ParserError> {
        self.must_consume(TokenType::LeftParen, "Expect ( after for")?;

        let init = if self.take_if_matches(TokenType::Semicolon).is_some() {
            None
        } else if self.take_if_matches(TokenType::Var).is_some() {
            Some(self.var_decl()?)
        } else {
            Some(self.expression_stmt()?)
        };

        let cond = if self.take_if_matches(TokenType::Semicolon).is_none() {
            self.expression()?
        } else {
            Expr::Literal(LiteralValue::True)
        };
        self.must_consume(TokenType::Semicolon, "Expect ; after loop condition")?;

        let inc = if self.take_if_matches(TokenType::RightParen).is_none() {
            Some(self.expression()?)
        } else {
            None
        };
        self.must_consume(TokenType::RightParen, "Expect ) after for clauses")?;

        let mut body = self.statement()?;

        if let Some(inc) = inc {
            body = Stmt::Block(vec![body, Stmt::Expression(inc)])
        }

        body = Stmt::While(cond, Box::new(body));

        if let Some(init) = init {
            body = Stmt::Block(vec![init, body]);
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt, ParserError> {
        self.must_consume(TokenType::LeftParen, "Expect ( after while")?;
        let cond = self.expression()?;
        self.must_consume(TokenType::RightParen, "Expect ) after condition")?;
        let body = self.statement()?;

        Ok(Stmt::While(cond, Box::new(body)))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.must_consume(TokenType::LeftParen, "Expect '(' after if")?;
        let cond = self.expression()?;
        self.must_consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then = Box::new(self.statement()?);

        let elze = if self.take_if_matches(TokenType::Else).is_some() {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If { cond, then, elze })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = Vec::new();

        while !self.next_is(TokenType::RightBrace) && !self.at_end() {
            stmts.push(self.declaration()?)
        }
        self.must_consume(TokenType::RightBrace, "Expect '}' after block")?;

        Ok(stmts)
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParserError> {
        let value = self.expression()?;
        self.must_consume(TokenType::Semicolon, "Expected ';' after value.")?;
        Ok(Stmt::Print(value))
    }

    fn expression_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        self.must_consume(TokenType::Semicolon, "Expected ';' after expression.")?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.or()?;

        if let Some(equals) = self.take_if_matches(TokenType::Equal) {
            let value = self.assignment()?;

            if let Expr::Variable(name) = &expr {
                return Ok(Expr::Assign {
                    name: name.clone(),
                    value: Box::new(value),
                });
            }

            return Err(ParserError::UnexpectedToken(equals));
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.and()?;

        while let Some(op) = self.take_if_matches(TokenType::Or) {
            let right = Box::new(self.and()?);
            expr = Expr::Logical {
                left: Box::new(expr),
                op,
                right,
            }
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.equality()?;

        while let Some(op) = self.take_if_matches(TokenType::And) {
            let right = Box::new(self.equality()?);
            expr = Expr::Logical {
                left: Box::new(expr),
                op,
                right,
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while let Some(op) = self.take_if_matches_one(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
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
        while let Some(op) = self.take_if_matches_one(vec![
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
        while let Some(op) = self.take_if_matches_one(vec![TokenType::Minus, TokenType::Plus]) {
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
        while let Some(op) = self.take_if_matches_one(vec![TokenType::Slash, TokenType::Star]) {
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
        if let Some(op) = self.take_if_matches_one(vec![TokenType::Bang, TokenType::Minus]) {
            let right = Box::new(self.unary()?);
            return Ok(Expr::Unary { op, right });
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;

        while self.take_if_matches(TokenType::LeftParen).is_some() {
            expr = self.finish_call(expr)?;
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        let mut args = Vec::new();
        if !self.next_is(TokenType::RightParen) {
            loop {
                if args.len() >= 255 {
                    return Err(ParserError::TooManyFnArguments);
                }
                args.push(self.expression()?);
                if self.take_if_matches(TokenType::Comma).is_none() {
                    break;
                }
            }
        }

        let paren = self.must_consume(TokenType::RightParen, "Expect ')' after arguments.")?;

        Ok(Expr::Call {
            callee: Box::new(callee),
            paren,
            args,
        })
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if self.take_if_matches(TokenType::False).is_some() {
            return Ok(Expr::Literal(LiteralValue::False));
        }
        if self.take_if_matches(TokenType::True).is_some() {
            return Ok(Expr::Literal(LiteralValue::True));
        }
        if self.take_if_matches(TokenType::Nil).is_some() {
            return Ok(Expr::Literal(LiteralValue::Nil));
        }
        if let Some(tok) = self.take_if_matches_one(vec![TokenType::Number, TokenType::String]) {
            return Ok(Expr::Literal(
                tok.literal.expect("expected Number or String literal"),
            ));
        }
        if let Some(identifier) = self.take_if_matches(TokenType::Identifier) {
            return Ok(Expr::Variable(identifier));
        }
        if self.take_if_matches(TokenType::LeftParen).is_some() {
            let expr = self.expression()?;
            let _ = self.must_consume(TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping {
                expr: Box::new(expr),
            });
        }

        match self.tokens.next() {
            Some(tok) => Err(ParserError::UnexpectedToken(tok)),
            None => Err(ParserError::UnexpectedEOF),
        }
    }

    fn must_consume(&mut self, tt: TokenType, err: &str) -> Result<Token, ParserError> {
        if self.next_is(tt) {
            Ok(self.tokens.next().unwrap())
        } else {
            Err(ParserError::ExpectedTokenNotFound(err.to_string()))
        }
    }

    fn take_if_matches(&mut self, t: TokenType) -> Option<Token> {
        self.take_if_matches_one(iter::once(t))
    }

    fn take_if_matches_one(&mut self, types: impl IntoIterator<Item = TokenType>) -> Option<Token> {
        self.tokens
            .next_if(|t| types.into_iter().any(|tt| t.typ == tt))
    }

    fn next_is(&mut self, tt: TokenType) -> bool {
        self.peek().filter(|&t| t.typ == tt).is_some()
    }

    fn at_end(&mut self) -> bool {
        self.next_is(TokenType::Eof)
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
