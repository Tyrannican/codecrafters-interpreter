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
                Op::Minus => {
                    if args.len() == 1 {
                        let Outcome::Number(n) = self.evaluate_statement(&args[0])? else {
                            anyhow::bail!("Operator must be a number");
                        };

                        return Ok(Outcome::Number(-n));
                    } else {
                        todo!("implement negation for two items");
                    }
                }
                Op::Bang => {
                    let outcome = match self.evaluate_statement(&args[0])? {
                        Outcome::String(_) | Outcome::Number(_) => Outcome::Boolean(false),
                        Outcome::Nil => Outcome::Boolean(true),
                        Outcome::Boolean(b) => Outcome::Boolean(!b),
                    };

                    return Ok(outcome);
                }
                Op::Star | Op::Slash => {
                    assert!(args.len() > 1);
                    let lhs = self.evaluate_statement(&args[0])?;
                    let rhs = self.evaluate_statement(&args[1])?;

                    // TODO: extract separately, we need the boolean check for concatenation of
                    // string
                    let (left, right) = self.number_check(lhs, rhs)?;

                    if *op == Op::Star {
                        return Ok(Outcome::Number(left * right));
                    } else {
                        return Ok(Outcome::Number(left / right));
                    }
                }
                _ => todo!("implement operation: {op}"),
            },
            other => todo!("need to implement {other}"),
        }
    }

    fn number_check(&self, left: Outcome<'de>, right: Outcome<'de>) -> Result<(f64, f64)> {
        if !matches!(left, Outcome::Number(_)) && !matches!(right, Outcome::Number(_)) {
            anyhow::bail!("Operands must be numbers: {left} {right}");
        }

        let Outcome::Number(left) = left else {
            unreachable!("checked above");
        };

        let Outcome::Number(right) = right else {
            unreachable!("checked above");
        };

        Ok((left, right))
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
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
