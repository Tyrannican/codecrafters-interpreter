use crate::lex::{Atom, Lexer, Op, TokenClass, TokenType};
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
        self.parse_statement(0)
    }

    pub fn parse_statement(&mut self, min_bp: u8) -> Result<Ast<'de>, Error> {
        let mut lhs = match self.lexer.next() {
            Some(Ok(token)) => match token.class() {
                Some(TokenClass::Atom(atom)) => Ast::Atom(atom),
                Some(TokenClass::Op(op)) => match op {
                    Op::Group => {
                        let rhs = self.parse_statement(0)?;
                        self.lexer.expect(TokenType::RightParen)?;
                        Ast::Cons(op, vec![rhs])
                    }
                    Op::Minus | Op::Bang | Op::Return | Op::Print => {
                        let ((), rbp) = prefix_binding_power(op);
                        let rhs = self.parse_statement(rbp)?;
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
            let rhs = self.parse_statement(rbp)?;
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
        Op::Plus | Op::Minus => (1, 2),
        Op::Star | Op::Slash => (3, 4),
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
        }
    }
}
