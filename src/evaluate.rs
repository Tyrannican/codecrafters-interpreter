use crate::{
    lex::{Atom, Op},
    parse::Ast,
};

use anyhow::Result;

pub struct Evaluator<'de> {
    ast: &'de Ast<'de>,
}

impl<'de> Evaluator<'de> {
    pub fn new(ast: &'de Ast<'de>) -> Self {
        Self { ast }
    }

    pub fn evaluate(&mut self) -> Result<()> {
        let Ast::Program(statements) = self.ast else {
            anyhow::bail!("parse error - expected program");
        };

        for statement in statements {
            let res = self.evaluate_statement(statement)?;

            println!("{res}");
        }

        Ok(())
    }

    fn evaluate_statement(&mut self, ast: &'de Ast<'de>) -> Result<Outcome<'de>> {
        match ast {
            Ast::Atom(atom) => match atom {
                Atom::String(s) => return Ok(Outcome::String(*s)),
                Atom::Number(n) => return Ok(Outcome::Number(*n)),
                Atom::Bool(b) => return Ok(Outcome::Boolean(*b)),
                Atom::Nil => return Ok(Outcome::Nil),
                _ => unimplemented!("not yet"),
            },
            Ast::Cons(op, args) => match op {
                Op::Group => {
                    let lhs = &args[0];
                    return self.evaluate_statement(lhs);
                }
                _ => todo!("implement operation: {op}"),
            },
            other => todo!("need to implement {other}"),
        }
    }
}

enum Outcome<'de> {
    String(&'de str),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl<'de> std::fmt::Display for Outcome<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{}", s.trim_matches('"')),
            Self::Number(n) => {
                if *n == n.trunc() {
                    write!(f, "{}", n.trunc())
                } else {
                    write!(f, "{n}")
                }
            }
            Self::Boolean(b) => write!(f, "{b:?}"),
            Self::Nil => write!(f, "nil"),
        }
    }
}
