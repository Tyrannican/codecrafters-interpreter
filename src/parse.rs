use crate::lex::{Atom, Lexer, Op, Token, TokenClass, TokenType};
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
        let mut statements = Vec::new();
        self.parse_program()?;
        // loop {
        //     statements.push(self.parse_statement(0)?);
        //     if self.lexer.is_empty() {
        //         break;
        //     }

        //     self.lexer.expect(TokenType::Semicolon)?;
        // }

        Ok(Ast::Program(statements))
    }

    fn parse_program(&mut self) -> Result<(), Error> {
        let mut statements = Vec::new();
        match self.lexer.next() {
            Some(Ok(token)) => match token.subtype {
                TokenType::Fun => statements.push(self.parse_function()?),
                TokenType::Class => statements.push(self.parse_class()?),
                _ => statements.push(self.parse_statement()?),
            },
            Some(Err(e)) => anyhow::bail!("syntax error: {}", e.to_string()),
            None => {}
        }

        todo!()
    }

    fn parse_statement(&mut self) -> Result<Ast<'de>, Error> {
        todo!()
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
                func_params.push(param);

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
        todo!()
    }

    fn parse_block(&mut self) -> Result<Ast<'de>, Error> {
        self.lexer.expect(TokenType::LeftBrace)?;
        self.lexer.expect(TokenType::RightBrace)?;

        Ok(Ast::Block(Vec::new()))
    }

    pub fn parse_statement_old(&mut self, min_bp: u8) -> Result<Ast<'de>, Error> {
        let mut lhs = match self.lexer.next() {
            Some(Ok(token)) => match token.class() {
                Some(TokenClass::Atom(atom)) => Ast::Atom(atom),
                Some(TokenClass::Op(op)) => match op {
                    Op::Group => {
                        let rhs = self.parse_statement_old(0)?;
                        self.lexer.expect(TokenType::RightParen)?;
                        Ast::Cons(op, vec![rhs])
                    }
                    Op::Minus | Op::Bang | Op::Return | Op::Print => {
                        let ((), rbp) = prefix_binding_power(op);
                        let rhs = self.parse_statement_old(rbp)?;
                        Ast::Cons(op, vec![rhs])
                    }
                    _ => anyhow::bail!("syntax error"),
                },
                None => anyhow::bail!("expected atom - found None"),
            },
            None => return Ok(Ast::Atom(Atom::Nil)),
            Some(Err(_)) => anyhow::bail!("syntax error"),
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                let err = self.lexer.next().expect("checked").expect_err("checked");
                anyhow::bail!("encountered op parse error: {err}");
            }

            let op = match op.map(|res| res.as_ref().expect("checked err above")) {
                Some(token) => match token.class() {
                    Some(TokenClass::Op(op)) => op,
                    None => break,
                    _ => panic!("OP: {op:?}"),
                },
                None => break,
            };

            let (lbp, rbp) = infix_binding_power(op);
            if lbp < min_bp {
                break;
            }
            self.lexer.next();
            let rhs = self.parse_statement_old(rbp)?;
            lhs = Ast::Cons(op, vec![lhs, rhs])
        }

        Ok(lhs)
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Print | Op::Return => ((), 1),
        Op::Minus | Op::Bang => ((), 5),
        _ => panic!("bad op: {op:?}"),
    }
}

fn infix_binding_power(op: Op) -> (u8, u8) {
    match op {
        Op::Assign => (2, 1),
        Op::Plus | Op::Minus => (3, 4),
        Op::Star | Op::Slash => (5, 6),
        Op::LessEqual
        | Op::Less
        | Op::GreaterEqual
        | Op::Greater
        | Op::BangEqual
        | Op::EqualEqual => (9, 10),
        Op::Field => (8, 7),
        _ => panic!("infix bad op: {op:?}"),
    }
}

pub enum Ast<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<Ast<'de>>),
    Function {
        name: Atom<'de>,
        parameters: Vec<Token<'de>>,
        block: Box<Ast<'de>>,
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
            Self::Block(block) => {
                for item in block {
                    write!(f, "{item}")?;
                }

                Ok(())
            }
            Self::Program(tree) => {
                for entry in tree.iter() {
                    write!(f, "{entry} ")?;
                }

                Ok(())
            }
        }
    }
}
