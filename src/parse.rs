use crate::lex::{Atom, Lexer, Op, TokenClass};
use anyhow::{Error, Result};

#[derive(Debug)]
pub struct Parser<'de> {
    input: &'de str,
    lexer: Lexer<'de>,
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(&mut self) -> Result<Expr<'de>, Error> {
        self.parse_bp(0)
    }

    fn parse_bp(&mut self, min_bp: u8) -> Result<Expr<'de>, Error> {
        let mut lhs = match self.lexer.next() {
            Some(Ok(token)) => match token.class() {
                Some(TokenClass::Atom(atom)) => Expr::Atom(atom),
                Some(TokenClass::Op(op)) => {
                    let ((), rbp) = prefix_binding_power(op);
                    let rhs = self.parse_bp(rbp)?;
                    Expr::Cons(op, vec![rhs])
                }
                None => anyhow::bail!("expected atom - found None"),
            },
            None => return Ok(Expr::Atom(Atom::Nil)),
            Some(Err(e)) => return Err(e.into()),
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
                    _ => todo!(),
                },
                None => break,
            };

            let (lbp, rbp) = infix_binding_power(op);
            if lbp < min_bp {
                break;
            }
            self.lexer.next();
            let rhs = self.parse_bp(rbp)?;
            lhs = Expr::Cons(op, vec![lhs, rhs])
        }

        Ok(lhs)
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Minus | Op::Bang => ((), 5),
        _ => todo!(),
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
        | Op::EqualEqual => (5, 6),
        Op::Field => (8, 7),
        _ => todo!(),
    }
}

pub enum Expr<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<Expr<'de>>),
}

impl<'de> std::fmt::Display for Expr<'de> {
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
