use std::borrow::Cow;

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
                Atom::String(s) => return Ok(Outcome::String(Cow::Borrowed(s.trim_matches('"')))),
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
                        let lhs = self.evaluate_statement(&args[0])?;
                        let rhs = self.evaluate_statement(&args[1])?;
                        if !self.check_numbers(&lhs, &rhs) {
                            anyhow::bail!("operands must be numbers: {lhs} {rhs}");
                        }
                        let Outcome::Number(left) = lhs else {
                            unreachable!("checked above");
                        };

                        let Outcome::Number(right) = rhs else {
                            unreachable!("checked above");
                        };

                        return Ok(Outcome::Number(left - right));
                    }
                }

                Op::Plus => {
                    let lhs = self.evaluate_statement(&args[0])?;
                    let rhs = self.evaluate_statement(&args[1])?;

                    if self.check_numbers(&lhs, &rhs) {
                        let Outcome::Number(left) = lhs else {
                            unreachable!("checked above");
                        };

                        let Outcome::Number(right) = rhs else {
                            unreachable!("checked above");
                        };

                        return Ok(Outcome::Number(left + right));
                    } else if self.check_strings(&lhs, &rhs) {
                        let Outcome::String(left) = lhs else {
                            unreachable!("checked above");
                        };

                        let Outcome::String(right) = rhs else {
                            unreachable!("checked above");
                        };

                        return Ok(Outcome::String(Cow::Owned(format!("{left}{right}"))));
                    } else {
                        anyhow::bail!("operands must be either numbers or strings {lhs} {rhs}");
                    }
                }

                Op::GreaterEqual | Op::Greater | Op::Less | Op::LessEqual => {
                    let lhs = self.evaluate_statement(&args[0])?;
                    let rhs = self.evaluate_statement(&args[1])?;

                    // TODO: Check strings for == and !=:
                    if !self.check_numbers(&lhs, &rhs) {
                        anyhow::bail!("operands must be numbers {lhs} {rhs}");
                    }

                    let Outcome::Number(left) = lhs else {
                        unreachable!("checked above");
                    };

                    let Outcome::Number(right) = rhs else {
                        unreachable!("checked above");
                    };

                    match op {
                        Op::GreaterEqual => return Ok(Outcome::Boolean(left >= right)),
                        Op::Greater => return Ok(Outcome::Boolean(left > right)),
                        Op::LessEqual => return Ok(Outcome::Boolean(left <= right)),
                        Op::Less => return Ok(Outcome::Boolean(left < right)),
                        _ => unreachable!("checked above"),
                    }
                }

                Op::EqualEqual | Op::BangEqual => {
                    let lhs = self.evaluate_statement(&args[0])?;
                    let rhs = self.evaluate_statement(&args[1])?;

                    if self.check_numbers(&lhs, &rhs) {
                        let Outcome::Number(left) = lhs else {
                            unreachable!("checked above");
                        };

                        let Outcome::Number(right) = rhs else {
                            unreachable!("checked above");
                        };

                        if *op == Op::EqualEqual {
                            return Ok(Outcome::Boolean(left == right));
                        } else {
                            return Ok(Outcome::Boolean(left != right));
                        }
                    } else if self.check_strings(&lhs, &rhs) {
                        let Outcome::String(left) = lhs else {
                            unreachable!("checked above");
                        };

                        let Outcome::String(right) = rhs else {
                            unreachable!("checked above");
                        };

                        if *op == Op::EqualEqual {
                            return Ok(Outcome::Boolean(left == right));
                        } else {
                            return Ok(Outcome::Boolean(left != right));
                        }
                    }

                    Ok(Outcome::Boolean(false))
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

                    if !self.check_numbers(&lhs, &rhs) {
                        anyhow::bail!("operands must be numbers: {lhs} {rhs}");
                    }
                    let Outcome::Number(left) = lhs else {
                        unreachable!("checked above");
                    };

                    let Outcome::Number(right) = rhs else {
                        unreachable!("checked above");
                    };

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

    // TODO: Move outside
    fn check_numbers(&self, left: &Outcome<'de>, right: &Outcome<'de>) -> bool {
        matches!(left, Outcome::Number(_)) && matches!(right, Outcome::Number(_))
    }

    fn check_strings(&self, left: &Outcome<'de>, right: &Outcome<'de>) -> bool {
        matches!(left, Outcome::String(_)) && matches!(right, Outcome::String(_))
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Outcome<'de> {
    String(Cow<'de, str>),
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
