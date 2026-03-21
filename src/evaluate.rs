use crate::{
    lex::{Atom, Op},
    parse::Ast,
};

use anyhow::Result;

pub struct Evaluator<'de> {
    ast: Ast<'de>,
}

impl<'de> Evaluator<'de> {
    pub fn new(ast: Ast<'de>) -> Self {
        Self { ast }
    }

    pub fn evaluate(&mut self) -> Result<()> {
        match self.ast {
            Ast::Cons(op, ref args) => self.parse_operation(op, args),
            Ast::Atom(atom) => self.parse_atom(atom),
        }

        Ok(())
    }

    fn parse_atom(&self, atom: Atom) {
        match atom {
            Atom::Ident(_) => {}
            Atom::Number(n) => {
                if n == n.trunc() {
                    println!("{}", n as isize);
                } else {
                    println!("{n}");
                }
            }
            atom => println!("{atom}"),
        }
    }

    fn parse_operation(&self, op: Op, args: &[Ast]) {
        todo!()
    }
}
