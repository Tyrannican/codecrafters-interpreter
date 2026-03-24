use crate::lex::{Atom, Lexer, Op, Token, TokenType};
use anyhow::{Error, Result};

#[derive(Debug)]
pub struct Parser<'de> {
    lexer: Lexer<'de>,
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(&mut self) -> Result<Ast<'de>, Error> {
        let statements = self.parse_program()?;
        Ok(Ast::Program(statements))
    }

    fn parse_program(&mut self) -> Result<Vec<Ast<'de>>, Error> {
        let mut statements = Vec::new();
        while !self.lexer.is_empty() {
            statements.push(self.parse_statement(0)?);
        }

        Ok(statements)
    }

    fn parse_function(&mut self) -> Result<Ast<'de>, Error> {
        let next = self.lexer.expect(TokenType::Ident)?;
        let name = Atom::Ident(next.source);
        self.lexer.expect(TokenType::LeftParen)?;
        let mut func_params = Vec::new();
        if !matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                subtype: TokenType::RightParen,
                ..
            }))
        ) {
            loop {
                let param = self.lexer.expect(TokenType::Ident)?;
                func_params.push(Ast::Atom(Atom::Ident(param.source)));

                match self.lexer.next() {
                    Some(Ok(Token {
                        subtype: TokenType::Comma,
                        ..
                    })) => continue,
                    Some(Ok(Token {
                        subtype: TokenType::RightParen,
                        ..
                    })) => break,
                    _ => anyhow::bail!("syntax error in function declaration"),
                }
            }
        }

        let block = self.parse_block()?;
        Ok(Ast::Function {
            name,
            parameters: func_params,
            block: Box::new(block),
        })
    }

    fn parse_class(&mut self) -> Result<Ast<'de>, Error> {
        // https://github.com/jonhoo/lox/blob/master/src/parse.rs#L197
        unimplemented!("not yet")
    }

    fn parse_function_call(&mut self) -> Result<Vec<Ast<'de>>, Error> {
        let mut args = Vec::new();

        if matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                subtype: TokenType::RightParen,
                ..
            }))
        ) {
            //
        } else {
            loop {
                let arg = self.parse_expression(0)?;
                args.push(arg);

                let token = self
                    .lexer
                    .expect_choice(&[TokenType::Comma, TokenType::RightParen])?;
                if token.subtype == TokenType::RightParen {
                    break;
                }
            }
        }

        Ok(args)
    }

    fn parse_block(&mut self) -> Result<Ast<'de>, Error> {
        let mut statements = Vec::new();
        self.lexer.expect(TokenType::LeftBrace)?;
        while !matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                subtype: TokenType::RightBrace,
                ..
            })),
        ) {
            statements.push(self.parse_statement(0)?);
        }
        self.lexer.expect(TokenType::RightBrace)?;

        Ok(Ast::Block(statements))
    }

    fn parse_statement(&mut self, min_bp: u8) -> Result<Ast<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(Ast::Atom(Atom::Nil)),
            Some(Err(e)) => return Err(e.into()),
        };

        let mut lhs = match lhs.subtype {
            TokenType::Ident => Ast::Atom(Atom::Ident(lhs.source)),
            TokenType::Super => Ast::Atom(Atom::Super),
            TokenType::This => Ast::Atom(Atom::This),
            TokenType::LeftParen => {
                let lhs = self.parse_expression(0)?;
                self.lexer.expect(TokenType::RightParen)?;
                Ast::Cons(Op::Group, vec![lhs])
            }
            TokenType::Print | TokenType::Return => {
                let op = match lhs.subtype {
                    TokenType::Print => Op::Print,
                    TokenType::Return => Op::Return,
                    _ => unreachable!("by the match pattern outside"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expression(r_bp)?;
                return Ok(Ast::Cons(op, vec![rhs]));
            }
            TokenType::For => {
                self.lexer.expect(TokenType::LeftParen)?;
                let initialiser = self.parse_expression(0)?;
                self.lexer.expect(TokenType::Semicolon)?;
                let condition = self.parse_expression(0)?;
                self.lexer.expect(TokenType::Semicolon)?;
                let increment = self.parse_expression(0)?;
                self.lexer.expect(TokenType::RightParen)?;
                let block = self.parse_block()?;

                return Ok(Ast::Cons(
                    Op::For,
                    vec![initialiser, condition, increment, block],
                ));
            }
            TokenType::While => {
                self.lexer.expect(TokenType::LeftParen)?;
                let condition = self.parse_expression(0)?;
                self.lexer.expect(TokenType::RightParen)?;

                let block = self.parse_block()?;
                return Ok(Ast::Cons(Op::While, vec![condition, block]));
            }
            TokenType::Class => return self.parse_class(),
            TokenType::Fun => return self.parse_function(),
            TokenType::Var => {
                let token = self.lexer.expect(TokenType::Ident)?;
                let ident = Ast::Atom(Atom::Ident(token.source));
                self.lexer.expect(TokenType::Equal)?;
                let assignment = self.parse_expression(0)?;
                return Ok(Ast::Cons(Op::Var, vec![ident, assignment]));
            }
            TokenType::If => {
                self.lexer.expect(TokenType::LeftParen)?;
                let condition = self.parse_expression(0)?;
                self.lexer.expect(TokenType::RightParen)?;
                let block = self.parse_block()?;
                let mut other = None;

                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        subtype: TokenType::Else,
                        ..
                    }))
                ) {
                    self.lexer.next();
                    other = Some(self.parse_block()?);
                }

                return Ok(Ast::If {
                    condition: Box::new(condition),
                    yes: Box::new(block),
                    no: other.map(Box::new),
                });
            }
            _ => self.parse_top_level_expression(&lhs, 0)?,
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                anyhow::bail!("unexpected eof???");
            }

            let op = match op.map(|res| res.as_ref().expect("handled above")) {
                None => break,
                Some(Token {
                    subtype: TokenType::LeftParen,
                    ..
                }) => Op::Call,
                Some(Token {
                    subtype: TokenType::Dot,
                    ..
                }) => Op::Field,
                Some(token) => token.operation()?,
            };

            if let Some((lbp, ())) = postfix_binding_power(op) {
                if lbp < min_bp {
                    break;
                }
                self.lexer.next();

                lhs = match op {
                    Op::Call => Ast::Call {
                        caller: Box::new(lhs),
                        arguments: self.parse_function_call()?,
                    },
                    _ => Ast::Cons(op, vec![lhs]),
                };
                continue;
            }

            if let Some((lbp, rbp)) = infix_binding_power(op) {
                if lbp < min_bp {
                    break;
                }
                self.lexer.next();

                let rhs = self.parse_statement(rbp)?;
                lhs = Ast::Cons(op, vec![lhs, rhs]);
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn parse_top_level_expression(
        &mut self,
        token: &Token<'de>,
        min_bp: u8,
    ) -> Result<Ast<'de>, Error> {
        let mut lhs = match token.subtype {
            TokenType::String => Ast::Atom(Atom::String(token.source)),
            TokenType::Number(n) => Ast::Atom(Atom::Number(n)),
            TokenType::True => Ast::Atom(Atom::Bool(true)),
            TokenType::False => Ast::Atom(Atom::Bool(false)),
            TokenType::Nil => Ast::Atom(Atom::Nil),
            TokenType::Ident => Ast::Atom(Atom::Ident(token.source)),
            TokenType::Super => Ast::Atom(Atom::Super),
            TokenType::This => Ast::Atom(Atom::This),
            TokenType::LeftParen => {
                let lhs = self.parse_expression(0)?;
                self.lexer.expect(TokenType::RightParen)?;
                Ast::Cons(Op::Group, vec![lhs])
            }
            TokenType::Bang | TokenType::Minus => {
                let op = match token.subtype {
                    TokenType::Bang => Op::Bang,
                    TokenType::Minus => Op::Minus,
                    _ => unreachable!("from the outer match pattern"),
                };

                let ((), rbp) = prefix_binding_power(op);
                let rhs = self.parse_expression(rbp)?;
                Ast::Cons(op, vec![rhs])
            }
            _ => anyhow::bail!("expected an expression - unexpected token {token:?}"),
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                let next = self
                    .lexer
                    .next()
                    .expect("checked above")
                    .expect_err("checked again");
                anyhow::bail!("expected operator - found {next:?}");
            }

            let op = match op.map(|res| res.as_ref().expect("handled above")) {
                None => break,
                Some(Token {
                    subtype:
                        TokenType::RightParen
                        | TokenType::Comma
                        | TokenType::Semicolon
                        | TokenType::RightBrace,
                    ..
                }) => break,
                Some(Token {
                    subtype: TokenType::LeftParen,
                    ..
                }) => Op::Call,
                Some(token) => token.operation()?,
            };

            if let Some((lbp, ())) = postfix_binding_power(op) {
                if lbp < min_bp {
                    break;
                }
                self.lexer.next();

                lhs = match op {
                    Op::Call => Ast::Call {
                        caller: Box::new(lhs),
                        arguments: self.parse_function_call()?,
                    },
                    _ => Ast::Cons(op, vec![lhs]),
                };

                continue;
            }

            if let Some((lbp, rbp)) = infix_binding_power(op) {
                if lbp < min_bp {
                    break;
                }

                self.lexer.next();
                let rhs = self.parse_expression(rbp)?;
                lhs = Ast::Cons(op, vec![lhs, rhs]);
                continue;
            }

            break;
        }

        if matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                subtype: TokenType::Semicolon,
                ..
            }))
        ) {
            self.lexer.next();
        }

        Ok(lhs)
    }

    fn parse_expression(&mut self, min_bp: u8) -> Result<Ast<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(Ast::Atom(Atom::Nil)),
            Some(Err(e)) => anyhow::bail!("expected expression - {}", e.to_string()),
        };

        let mut lhs = match lhs.subtype {
            TokenType::String => Ast::Atom(Atom::String(lhs.source)),
            TokenType::Number(n) => Ast::Atom(Atom::Number(n)),
            TokenType::True => Ast::Atom(Atom::Bool(true)),
            TokenType::False => Ast::Atom(Atom::Bool(false)),
            TokenType::Nil => Ast::Atom(Atom::Nil),
            TokenType::Ident => Ast::Atom(Atom::Ident(lhs.source)),
            TokenType::Super => Ast::Atom(Atom::Super),
            TokenType::This => Ast::Atom(Atom::This),
            TokenType::LeftParen => {
                let lhs = self.parse_expression(0)?;
                self.lexer.expect(TokenType::RightParen)?;
                Ast::Cons(Op::Group, vec![lhs])
            }
            TokenType::Bang | TokenType::Minus => {
                let op = match lhs.subtype {
                    TokenType::Bang => Op::Bang,
                    TokenType::Minus => Op::Minus,
                    _ => unreachable!("from the outer match pattern"),
                };

                let ((), rbp) = prefix_binding_power(op);
                let rhs = self.parse_expression(rbp)?;
                Ast::Cons(op, vec![rhs])
            }
            _ => anyhow::bail!("expected an expression - unexpected token {lhs:?}"),
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                let next = self
                    .lexer
                    .next()
                    .expect("checked above")
                    .expect_err("checked again");
                anyhow::bail!("expected operator - found {next:?}");
            }

            let op = match op.map(|res| res.as_ref().expect("handled above")) {
                None => break,
                Some(Token {
                    subtype:
                        TokenType::RightParen
                        | TokenType::Comma
                        | TokenType::Semicolon
                        | TokenType::RightBrace,
                    ..
                }) => break,
                Some(Token {
                    subtype: TokenType::LeftParen,
                    ..
                }) => Op::Call,
                Some(token) => token.operation()?,
            };

            if let Some((lbp, ())) = postfix_binding_power(op) {
                if lbp < min_bp {
                    break;
                }
                self.lexer.next();

                lhs = match op {
                    Op::Call => Ast::Call {
                        caller: Box::new(lhs),
                        arguments: self.parse_function_call()?,
                    },
                    _ => Ast::Cons(op, vec![lhs]),
                };

                continue;
            }

            if let Some((lbp, rbp)) = infix_binding_power(op) {
                if lbp < min_bp {
                    break;
                }

                self.lexer.next();
                let rhs = self.parse_expression(rbp)?;
                lhs = Ast::Cons(op, vec![lhs, rhs]);
                continue;
            }

            break;
        }

        if matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                subtype: TokenType::Semicolon,
                ..
            }))
        ) {
            self.lexer.next();
        }

        Ok(lhs)
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Print | Op::Return => ((), 1),
        Op::Minus | Op::Bang => ((), 11),
        _ => panic!("bad op: {op:?}"),
    }
}

fn infix_binding_power(op: Op) -> Option<(u8, u8)> {
    let res = match op {
        Op::Assign => (2, 1),
        Op::Plus | Op::Minus => (3, 4),
        Op::Star | Op::Slash => (7, 8),
        Op::LessEqual
        | Op::Less
        | Op::GreaterEqual
        | Op::Greater
        | Op::BangEqual
        | Op::EqualEqual => (9, 10),
        Op::Field => (16, 15),
        _ => return None,
    };

    Some(res)
}

fn postfix_binding_power(op: Op) -> Option<(u8, ())> {
    match op {
        Op::Call => Some((13, ())),
        _ => None,
    }
}

pub enum Ast<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<Ast<'de>>),
    Function {
        name: Atom<'de>,
        parameters: Vec<Ast<'de>>,
        block: Box<Ast<'de>>,
    },
    If {
        condition: Box<Ast<'de>>,
        yes: Box<Ast<'de>>,
        no: Option<Box<Ast<'de>>>,
    },
    Call {
        caller: Box<Ast<'de>>,
        arguments: Vec<Ast<'de>>,
    },
    Block(Vec<Ast<'de>>),
    Program(Vec<Ast<'de>>),
}

impl<'de> std::fmt::Display for Ast<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Atom(a) => write!(f, "{a}"),
            Self::Cons(op, rest) => {
                write!(f, "({op}")?;
                for r in rest {
                    write!(f, " {r}")?;
                }

                write!(f, ")")
            }
            Self::Function {
                name,
                parameters,
                block,
            } => {
                write!(f, "(fun {name}")?;
                for param in parameters {
                    write!(f, " {param}")?;
                }

                write!(f, "{block})")
            }
            Self::Call { caller, arguments } => {
                write!(f, "({caller}")?;
                for arg in arguments {
                    write!(f, " {arg}")?;
                }

                write!(f, ")")
            }
            Self::Program(tree) => {
                for entry in tree.iter() {
                    write!(f, "{entry}")?;
                }

                Ok(())
            }
            Self::If { condition, yes, no } => {
                write!(f, "(if {condition} {yes}")?;
                if let Some(no) = no {
                    write!(f, " {no}")?;
                }
                write!(f, ")")
            }
            Self::Block(statements) => {
                write!(f, "(block")?;
                for s in statements {
                    write!(f, " {s}")?;
                }
                write!(f, ")")
            }
        }
    }
}
