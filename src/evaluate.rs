use std::{borrow::Cow, collections::HashMap};

use crate::{
    lex::{Atom, Op},
    parse::Ast,
};

use anyhow::Result;

pub struct Evaluator<'de> {
    ast: &'de Ast<'de>,
    assignments: HashMap<Cow<'de, str>, Outcome<'de>>,
}

impl<'de> Evaluator<'de> {
    pub fn new(ast: &'de Ast<'de>) -> Self {
        Self {
            ast,
            assignments: HashMap::default(),
        }
    }

    pub fn evaluate(&mut self) -> Result<Vec<Outcome<'de>>> {
        let Ast::Program(statements) = self.ast else {
            anyhow::bail!("parse error - expected program");
        };

        let mut outputs = Vec::new();
        for statement in statements {
            outputs.push(self.evaluate_statement(statement)?);
        }

        Ok(outputs)
    }

    fn lookup_assignment(&self, ident: impl AsRef<str>) -> Option<Outcome<'de>> {
        if let Some(var) = self.assignments.get(ident.as_ref()) {
            return Some(var.clone());
        }

        None
    }

    fn evaluate_statement(&mut self, ast: &'de Ast<'de>) -> Result<Outcome<'de>> {
        let outcome = match ast {
            Ast::Atom(atom) => match atom {
                Atom::String(s) => Outcome::String(Cow::Borrowed(s.trim_matches('"'))),
                Atom::Number(n) => Outcome::Number(*n),
                Atom::Bool(b) => Outcome::Boolean(*b),
                Atom::Ident(i) => Outcome::Ident(Cow::Borrowed(*i)),
                Atom::Nil => Outcome::Nil,
                _ => unimplemented!("not yet"),
            },
            Ast::Cons(op, args) => match op {
                Op::Group => {
                    let lhs = &args[0];
                    return self.evaluate_statement(lhs);
                }

                Op::Assign => {
                    let lhs = self.evaluate_statement(&args[0])?;
                    let rhs = self.evaluate_statement(&args[1])?;

                    let Outcome::Ident(var) = lhs else {
                        return Ok(Outcome::create_error(
                            "can only assign to an identifier",
                            70,
                        ));
                    };

                    let rhs = match rhs {
                        Outcome::Ident(i) => {
                            if let Some(var) = self.lookup_assignment(&i) {
                                var
                            } else {
                                return Ok(Outcome::create_error(
                                    format!("undeclared variable: {var}"),
                                    70,
                                ));
                            }
                        }
                        _ => rhs,
                    };

                    if let Some(assignment) = self.assignments.get_mut(&var) {
                        *assignment = rhs.clone();
                        Outcome::Nil
                    } else {
                        Outcome::create_error(format!("undeclared variable: {var}"), 70)
                    }
                }

                Op::Var => {
                    let lhs = self.evaluate_statement(&args[0])?;
                    let rhs = self.evaluate_statement(&args[1])?;

                    let Outcome::Ident(var) = lhs else {
                        return Ok(Outcome::Error((
                            format!("assignment must be an identifier: {lhs}"),
                            70,
                        )));
                    };

                    let rhs = match rhs {
                        Outcome::Ident(i) => {
                            if let Some(var) = self.lookup_assignment(&i) {
                                var
                            } else {
                                return Ok(Outcome::create_error(
                                    format!("undeclared variable: {var}"),
                                    70,
                                ));
                            }
                        }
                        _ => rhs,
                    };

                    self.assignments.insert(var, rhs);
                    Outcome::Nil
                }

                Op::Minus => {
                    if args.len() == 1 {
                        let Outcome::Number(n) = self.evaluate_statement(&args[0])? else {
                            return Ok(Outcome::Error((format!("Operator must be a number"), 70)));
                        };

                        return Ok(Outcome::Number(-n));
                    } else {
                        let lhs = self.evaluate_statement(&args[0])?;
                        let rhs = self.evaluate_statement(&args[1])?;
                        if !self.check_numbers(&lhs, &rhs) {
                            return Ok(Outcome::Error((
                                format!("operands must be numbers: {lhs} {rhs}"),
                                70,
                            )));
                        }
                        let Outcome::Number(left) = lhs else {
                            unreachable!("checked above");
                        };

                        let Outcome::Number(right) = rhs else {
                            unreachable!("checked above");
                        };

                        Outcome::Number(left - right)
                    }
                }

                Op::Plus => {
                    let lhs = self.evaluate_statement(&args[0])?;
                    let rhs = self.evaluate_statement(&args[1])?;

                    let lhs = match lhs {
                        Outcome::Ident(i) => {
                            if let Some(var) = self.assignments.get(&i) {
                                var.clone()
                            } else {
                                return Ok(Outcome::create_error(
                                    format!("undeclared variable: {i}"),
                                    70,
                                ));
                            }
                        }
                        _ => lhs,
                    };

                    let rhs = match rhs {
                        Outcome::Ident(i) => {
                            if let Some(var) = self.assignments.get(&i) {
                                var.clone()
                            } else {
                                return Ok(Outcome::create_error(
                                    format!("undeclared variable: {i}"),
                                    70,
                                ));
                            }
                        }
                        _ => rhs,
                    };

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

                        Outcome::String(Cow::Owned(format!("{left}{right}")))
                    } else {
                        Outcome::Error((
                            format!(
                                "+ error: operands must be either numbers or strings {lhs} {rhs}"
                            ),
                            70,
                        ))
                    }
                }

                Op::GreaterEqual | Op::Greater | Op::Less | Op::LessEqual => {
                    let lhs = self.evaluate_statement(&args[0])?;
                    let rhs = self.evaluate_statement(&args[1])?;

                    if !self.check_numbers(&lhs, &rhs) {
                        return Ok(Outcome::Error((
                            format!("operands must be numbers {lhs} {rhs}"),
                            70,
                        )));
                    }

                    let Outcome::Number(left) = lhs else {
                        unreachable!("checked above");
                    };

                    let Outcome::Number(right) = rhs else {
                        unreachable!("checked above");
                    };

                    match op {
                        Op::GreaterEqual => Outcome::Boolean(left >= right),
                        Op::Greater => Outcome::Boolean(left > right),
                        Op::LessEqual => Outcome::Boolean(left <= right),
                        Op::Less => Outcome::Boolean(left < right),
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
                            Outcome::Boolean(left == right)
                        } else {
                            Outcome::Boolean(left != right)
                        }
                    } else if self.check_strings(&lhs, &rhs) {
                        let Outcome::String(left) = lhs else {
                            unreachable!("checked above");
                        };

                        let Outcome::String(right) = rhs else {
                            unreachable!("checked above");
                        };

                        if *op == Op::EqualEqual {
                            Outcome::Boolean(left == right)
                        } else {
                            Outcome::Boolean(left != right)
                        }
                    } else if self.check_booleans(&lhs, &rhs) {
                        let Outcome::Boolean(left) = lhs else {
                            unreachable!("checked above");
                        };

                        let Outcome::Boolean(right) = rhs else {
                            unreachable!("checked above");
                        };

                        if *op == Op::EqualEqual {
                            Outcome::Boolean(left == right)
                        } else {
                            Outcome::Boolean(left != right)
                        }
                    } else {
                        Outcome::Boolean(false)
                    }
                }

                Op::Bang => match self.evaluate_statement(&args[0])? {
                    Outcome::String(_) | Outcome::Number(_) => Outcome::Boolean(false),
                    Outcome::Nil => Outcome::Boolean(true),
                    Outcome::Boolean(b) => Outcome::Boolean(!b),
                    _ => anyhow::bail!("invalid"),
                },
                Op::Star | Op::Slash => {
                    assert!(args.len() > 1);
                    let lhs = self.evaluate_statement(&args[0])?;
                    let rhs = self.evaluate_statement(&args[1])?;

                    if !self.check_numbers(&lhs, &rhs) {
                        return Ok(Outcome::Error((
                            format!("operands must be numbers: {lhs} {rhs}"),
                            70,
                        )));
                    }
                    let Outcome::Number(left) = lhs else {
                        unreachable!("checked above");
                    };

                    let Outcome::Number(right) = rhs else {
                        unreachable!("checked above");
                    };

                    if *op == Op::Star {
                        Outcome::Number(left * right)
                    } else {
                        Outcome::Number(left / right)
                    }
                }

                Op::Print => {
                    let lhs = self.evaluate_statement(&args[0])?;
                    if lhs == Outcome::Nil {
                        Outcome::Error(("invalid print expression".to_string(), 70))
                    } else {
                        match lhs {
                            Outcome::Ident(var) => {
                                if let Some(assignment) = self.assignments.get(&var) {
                                    Outcome::Print(Box::new(assignment.clone()))
                                } else {
                                    Outcome::Error((format!("undefined variable: {var}"), 70))
                                }
                            }
                            _ => Outcome::Print(Box::new(lhs)),
                        }
                        // Outcome::Print(Box::new(lhs))
                    }
                }
                _ => todo!("implement operation: {op}"),
            },
            other => todo!("need to implement {other}"),
        };

        Ok(outcome)
    }

    // TODO: Move outside
    fn check_numbers(&self, left: &Outcome<'de>, right: &Outcome<'de>) -> bool {
        matches!(left, Outcome::Number(_)) && matches!(right, Outcome::Number(_))
    }

    fn check_strings(&self, left: &Outcome<'de>, right: &Outcome<'de>) -> bool {
        matches!(left, Outcome::String(_)) && matches!(right, Outcome::String(_))
    }

    fn check_booleans(&self, left: &Outcome<'de>, right: &Outcome<'de>) -> bool {
        matches!(left, Outcome::Boolean(_)) && matches!(right, Outcome::Boolean(_))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Outcome<'de> {
    String(Cow<'de, str>),
    Number(f64),
    Boolean(bool),
    Ident(Cow<'de, str>),
    Print(Box<Outcome<'de>>),
    Error((String, i32)),
    Nil,
}

impl<'de> Outcome<'de> {
    pub fn create_error(msg: impl AsRef<str>, code: i32) -> Self {
        Self::Error((msg.as_ref().to_string(), code))
    }
}

impl<'de> std::fmt::Display for Outcome<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) | Self::Ident(s) => write!(f, "{}", s.trim_matches('"')),
            Self::Number(n) => {
                if *n == n.trunc() {
                    write!(f, "{}", n.trunc())
                } else {
                    write!(f, "{n}")
                }
            }
            Self::Boolean(b) => write!(f, "{b:?}"),
            Self::Nil => write!(f, "nil"),
            Self::Print(statement) => write!(f, "{statement}"),
            Self::Error((msg, code)) => write!(f, "{msg}: {code}"),
        }
    }
}
