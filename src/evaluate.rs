use std::{borrow::Cow, cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    lex::{Atom, Op},
    parse::Ast,
};

use anyhow::Result;

pub struct Program<'de> {
    ast: &'de Ast<'de>,
    state: Rc<RefCell<Scope<'de>>>,
}

impl<'de> Program<'de> {
    pub fn new(ast: &'de Ast<'de>) -> Self {
        Self {
            ast,
            state: Rc::new(RefCell::new(Scope::new())),
        }
    }

    pub fn evaluate(&mut self) -> Result<Vec<Eval<'de>>> {
        let Ast::Program(statements) = self.ast else {
            anyhow::bail!("parse error - expected program");
        };

        let mut outputs = Vec::new();
        for statement in statements {
            match self.evaluate_statement(statement) {
                Ok(eval) => match eval {
                    Eval::Block(blk) => outputs.extend_from_slice(&blk),
                    other => outputs.push(other),
                },
                Err(e) => {
                    if let Some(eval) = e.downcast_ref::<Eval>() {
                        outputs.push(eval.clone());
                    } else {
                        anyhow::bail!("unknown error occurred: {}", e.to_string());
                    }
                }
            }
        }

        Ok(outputs)
    }

    fn evaluate_block_or_statement(&mut self, ast: &'de Ast<'de>) -> Result<Eval<'de>> {
        match ast {
            Ast::Block(blk) => self.evaluate_block(blk),
            _ => self.evaluate_statement_with_lookup(ast),
        }
    }

    fn evaluate_statement_with_lookup(&mut self, ast: &'de Ast<'de>) -> Result<Eval<'de>> {
        match self.evaluate_statement(ast)? {
            Eval::Ident(i) => {
                if let Some(var) = self.state.borrow().get(&i) {
                    Ok(var)
                } else {
                    Err(anyhow::anyhow!(Eval::create_error(
                        format!("undeclared variable: {i}"),
                        70
                    )))
                }
            }
            Eval::Return(ret) => match *ret {
                Eval::Ident(i) => {
                    if let Some(var) = self.state.borrow().get(&i) {
                        Ok(var)
                    } else {
                        Err(anyhow::anyhow!(Eval::create_error(
                            format!("undeclared variable: {i}"),
                            70
                        )))
                    }
                }
                other => Ok(other),
            },
            other => Ok(other),
        }
    }

    fn evaluate_statement(&mut self, ast: &'de Ast<'de>) -> Result<Eval<'de>> {
        let outcome = match ast {
            Ast::Atom(atom) => self.evaluate_atom(atom),
            Ast::Cons(op, args) => self.evaluate_cons(*op, args)?,
            Ast::Block(statements) => self.evaluate_block(statements)?,
            Ast::If { condition, yes, no } => self.evaluate_if(condition, yes, no)?,
            Ast::Call { caller, arguments } => self.evaluate_call(caller, arguments)?,
            Ast::Function {
                name,
                parameters,
                block,
            } => self.evaluate_function(*name, parameters, block)?,
            other => todo!("need to implement {other:?}"),
        };

        Ok(outcome)
    }

    fn evaluate_atom(&self, atom: &'de Atom) -> Eval<'de> {
        match atom {
            Atom::String(s) => Eval::String(Cow::Borrowed(s.trim_matches('"'))),
            Atom::Number(n) => Eval::Number(*n),
            Atom::Bool(b) => Eval::Boolean(*b),
            Atom::Ident(i) => Eval::Ident(Cow::Borrowed(*i)),
            Atom::Nil => Eval::Nil,
            _ => unimplemented!("not yet"),
        }
    }

    fn evaluate_cons(&mut self, op: Op, args: &'de [Ast<'de>]) -> Result<Eval<'de>> {
        let result = match op {
            Op::Group => {
                let lhs = &args[0];
                return self.evaluate_statement(lhs);
            }

            Op::Assign => {
                let lhs = self.evaluate_statement(&args[0])?;
                let rhs = self.evaluate_statement_with_lookup(&args[1])?;

                let Eval::Ident(var) = lhs else {
                    return Ok(Eval::create_error("can only assign to an identifier", 70));
                };

                if self.state.borrow_mut().assign(&var, rhs.clone()) {
                    rhs
                } else {
                    Eval::create_error(format!("undeclared variable: {var}"), 70)
                }
            }

            Op::Var => {
                let lhs = self.evaluate_statement(&args[0])?;
                let rhs = self.evaluate_statement_with_lookup(&args[1])?;

                let Eval::Ident(var) = lhs else {
                    return Ok(Eval::Error((
                        format!("assignment must be an identifier: {lhs}"),
                        70,
                    )));
                };

                self.state.borrow_mut().define(var, rhs);
                Eval::Nil
            }

            Op::Minus => {
                if args.len() == 1 {
                    let Eval::Number(n) = self.evaluate_statement_with_lookup(&args[0])? else {
                        return Ok(Eval::Error((format!("Operator must be a number"), 70)));
                    };

                    return Ok(Eval::Number(-n));
                } else {
                    let lhs = self.evaluate_statement_with_lookup(&args[0])?;
                    let rhs = self.evaluate_statement_with_lookup(&args[1])?;
                    if !self.check_numbers(&lhs, &rhs) {
                        return Ok(Eval::Error((
                            format!("operands must be numbers: {lhs} {rhs}"),
                            70,
                        )));
                    }
                    let Eval::Number(left) = lhs else {
                        unreachable!("checked above");
                    };

                    let Eval::Number(right) = rhs else {
                        unreachable!("checked above");
                    };

                    Eval::Number(left - right)
                }
            }

            Op::Plus => {
                let lhs = self.evaluate_statement_with_lookup(&args[0])?;
                let rhs = self.evaluate_statement_with_lookup(&args[1])?;

                if self.check_numbers(&lhs, &rhs) {
                    let Eval::Number(left) = lhs else {
                        unreachable!("checked above");
                    };

                    let Eval::Number(right) = rhs else {
                        unreachable!("checked above");
                    };

                    return Ok(Eval::Number(left + right));
                } else if self.check_strings(&lhs, &rhs) {
                    let Eval::String(left) = lhs else {
                        unreachable!("checked above");
                    };

                    let Eval::String(right) = rhs else {
                        unreachable!("checked above");
                    };

                    Eval::String(Cow::Owned(format!("{left}{right}")))
                } else {
                    Eval::Error((
                        format!("+ error: operands must be either numbers or strings {lhs} {rhs}"),
                        70,
                    ))
                }
            }

            Op::GreaterEqual | Op::Greater | Op::Less | Op::LessEqual => {
                let lhs = self.evaluate_statement_with_lookup(&args[0])?;
                let rhs = self.evaluate_statement_with_lookup(&args[1])?;

                if !self.check_numbers(&lhs, &rhs) {
                    return Ok(Eval::Error((
                        format!("operands must be numbers {lhs} {rhs}"),
                        70,
                    )));
                }

                let Eval::Number(left) = lhs else {
                    unreachable!("checked above");
                };

                let Eval::Number(right) = rhs else {
                    unreachable!("checked above");
                };

                match op {
                    Op::GreaterEqual => Eval::Boolean(left >= right),
                    Op::Greater => Eval::Boolean(left > right),
                    Op::LessEqual => Eval::Boolean(left <= right),
                    Op::Less => Eval::Boolean(left < right),
                    _ => unreachable!("checked above"),
                }
            }

            Op::EqualEqual | Op::BangEqual => {
                let lhs = self.evaluate_statement_with_lookup(&args[0])?;
                let rhs = self.evaluate_statement_with_lookup(&args[1])?;

                if self.check_numbers(&lhs, &rhs) {
                    let Eval::Number(left) = lhs else {
                        unreachable!("checked above");
                    };

                    let Eval::Number(right) = rhs else {
                        unreachable!("checked above");
                    };

                    if op == Op::EqualEqual {
                        Eval::Boolean(left == right)
                    } else {
                        Eval::Boolean(left != right)
                    }
                } else if self.check_strings(&lhs, &rhs) {
                    let Eval::String(left) = lhs else {
                        unreachable!("checked above");
                    };

                    let Eval::String(right) = rhs else {
                        unreachable!("checked above");
                    };

                    if op == Op::EqualEqual {
                        Eval::Boolean(left == right)
                    } else {
                        Eval::Boolean(left != right)
                    }
                } else if self.check_booleans(&lhs, &rhs) {
                    let Eval::Boolean(left) = lhs else {
                        unreachable!("checked above");
                    };

                    let Eval::Boolean(right) = rhs else {
                        unreachable!("checked above");
                    };

                    if op == Op::EqualEqual {
                        Eval::Boolean(left == right)
                    } else {
                        Eval::Boolean(left != right)
                    }
                } else {
                    Eval::Boolean(false)
                }
            }

            Op::Bang => match self.evaluate_statement_with_lookup(&args[0])? {
                Eval::String(_) | Eval::Number(_) => Eval::Boolean(false),
                Eval::Nil => Eval::Boolean(true),
                Eval::Boolean(b) => Eval::Boolean(!b),
                other => Eval::create_error(format!("invalid unary: {other}"), 70),
            },
            Op::Star | Op::Slash => {
                assert!(args.len() > 1);
                let lhs = self.evaluate_statement_with_lookup(&args[0])?;
                let rhs = self.evaluate_statement_with_lookup(&args[1])?;

                if !self.check_numbers(&lhs, &rhs) {
                    return Ok(Eval::Error((
                        format!("operands must be numbers: {lhs} {rhs}"),
                        70,
                    )));
                }
                let Eval::Number(left) = lhs else {
                    unreachable!("checked above");
                };

                let Eval::Number(right) = rhs else {
                    unreachable!("checked above");
                };

                if op == Op::Star {
                    Eval::Number(left * right)
                } else {
                    Eval::Number(left / right)
                }
            }

            Op::Print => {
                let lhs = self.evaluate_statement(&args[0])?;
                if lhs == Eval::Nil {
                    Eval::Error(("invalid print expression".to_string(), 70))
                } else {
                    match lhs {
                        Eval::Ident(var) => {
                            if let Some(assignment) = self.state.borrow().get(&var) {
                                Eval::Print(Box::new(assignment.clone()))
                            } else {
                                Eval::Error((format!("undefined variable: {var}"), 70))
                            }
                        }
                        _ => Eval::Print(Box::new(lhs)),
                    }
                }
            }

            Op::Or => {
                let lhs = self.evaluate_statement_with_lookup(&args[0])?;

                let left = match lhs {
                    Eval::Boolean(b) => b,
                    Eval::Nil => false,
                    _ => true,
                };

                if left {
                    lhs
                } else {
                    self.evaluate_statement_with_lookup(&args[1])?
                }
            }

            Op::And => {
                let lhs = self.evaluate_statement_with_lookup(&args[0])?;
                let left = match lhs {
                    Eval::Boolean(b) => b,
                    Eval::Nil => false,
                    _ => true,
                };

                if !left {
                    lhs
                } else {
                    self.evaluate_statement_with_lookup(&args[1])?
                }
            }

            Op::While => {
                self.enter_scope();
                let lhs = self.evaluate_statement_with_lookup(&args[0])?;
                let mut cond = match lhs {
                    Eval::Boolean(b) => b,
                    Eval::Nil => false,
                    _ => true,
                };

                let mut statements = Vec::new();
                while cond {
                    let rhs = self.evaluate_statement_with_lookup(&args[1])?;
                    match rhs {
                        Eval::Block(blk) => statements.extend_from_slice(&blk),
                        other => statements.push(other),
                    }
                    let lhs = self.evaluate_statement_with_lookup(&args[0])?;
                    cond = match lhs {
                        Eval::Boolean(b) => b,
                        Eval::Nil => false,
                        _ => true,
                    };
                }
                self.exit_scope();

                Eval::Block(statements)
            }

            Op::For => {
                self.enter_scope();
                self.evaluate_statement_with_lookup(&args[0])?;
                let cond = self.evaluate_statement_with_lookup(&args[1])?;
                let mut cond = match cond {
                    Eval::Boolean(b) => b,
                    Eval::Nil => false,
                    _ => true,
                };

                let mut statements = Vec::new();
                while cond {
                    match self.evaluate_statement_with_lookup(&args[3])? {
                        Eval::Block(blk) => statements.extend_from_slice(&blk),
                        other => statements.push(other),
                    }

                    self.evaluate_statement_with_lookup(&args[2])?;
                    let check = self.evaluate_statement_with_lookup(&args[1])?;
                    cond = match check {
                        Eval::Boolean(b) => b,
                        Eval::Nil => false,
                        _ => true,
                    };
                }
                self.exit_scope();

                Eval::Block(statements)
            }

            Op::Return => Eval::Return(Box::new(self.evaluate_statement_with_lookup(&args[0])?)),

            _ => todo!("implement operation: {op}"),
        };

        Ok(result)
    }

    fn evaluate_block(&mut self, ast: &'de [Ast<'de>]) -> Result<Eval<'de>> {
        self.enter_scope();
        let mut outputs = Vec::new();
        for statement in ast {
            match self.evaluate_statement_with_lookup(statement)? {
                Eval::Block(blk) => outputs.extend_from_slice(&blk),
                Eval::Return(ret) => {
                    outputs.push(*ret);
                    return Ok(Eval::Block(outputs));
                }
                other => outputs.push(other),
            }
        }
        self.exit_scope();
        Ok(Eval::Block(outputs))
    }

    fn evaluate_if(
        &mut self,
        condition: &'de Box<Ast<'de>>,
        yes: &'de Box<Ast<'de>>,
        no: &'de Option<Box<Ast<'de>>>,
    ) -> Result<Eval<'de>> {
        self.enter_scope();
        let pass = match self.evaluate_statement_with_lookup(condition)? {
            Eval::Boolean(b) => b,
            Eval::Nil => false,
            _ => true,
        };

        let resolution = if pass {
            self.evaluate_block_or_statement(yes)?
        } else {
            match no {
                Some(else_block) => self.evaluate_block_or_statement(else_block)?,
                None => Eval::Nil,
            }
        };

        self.exit_scope();

        Ok(resolution)
    }

    fn evaluate_function(
        &mut self,
        name: Atom<'de>,
        func_args: &'de [Ast<'de>],
        block: &'de Box<Ast<'de>>,
    ) -> Result<Eval<'de>> {
        let Atom::Ident(func_name) = name else {
            anyhow::bail!("can only call functions or classes");
        };

        let mut args = vec![];
        for arg in func_args.into_iter() {
            let Ast::Atom(Atom::Ident(ident)) = arg else {
                anyhow::bail!("expected identifier");
            };

            args.push(Eval::Ident(Cow::Borrowed(*ident)));
        }

        let f = Eval::Function {
            func_name: Cow::Borrowed(func_name),
            args,
            block,
        };
        self.state.borrow_mut().define(&func_name, f);
        Ok(Eval::Nil)
    }

    fn evaluate_call(
        &mut self,
        caller: &'de Box<Ast<'de>>,
        call_args: &'de [Ast<'de>],
    ) -> Result<Eval<'de>> {
        match self.native_function(caller)? {
            Eval::Nil => {}
            ret => return Ok(ret),
        }

        let Eval::Function {
            func_name: _,
            args,
            block,
        } = self.evaluate_statement_with_lookup(caller)?
        else {
            anyhow::bail!("can only call functions or classes");
        };

        assert!(args.len() == call_args.len());

        let call_args: Vec<Eval<'de>> = call_args
            .into_iter()
            .map(|a| {
                self.evaluate_statement_with_lookup(a)
                    .expect("these should be values or idents")
            })
            .collect();

        for (ident, value) in args.into_iter().zip(call_args.into_iter()) {
            let Eval::Ident(ident) = ident else {
                anyhow::bail!("expected identifier");
            };
            self.state.borrow_mut().define(ident, value);
        }

        let value = self.evaluate_block_or_statement(block)?;

        Ok(value)
    }

    fn native_function(&mut self, caller: &'de Box<Ast<'de>>) -> Result<Eval<'de>> {
        let Eval::Ident(ident) = self.evaluate_statement(caller)? else {
            todo!()
        };

        match ident.as_ref() {
            "clock" => return Ok(Eval::Number(jiff::Timestamp::now().as_second() as f64)),
            _ => Ok(Eval::Nil),
        }
    }

    fn enter_scope(&mut self) {
        let child = Scope {
            assignments: HashMap::new(),
            parent: Some(Rc::clone(&self.state)),
        };

        self.state = Rc::new(RefCell::new(child));
    }

    fn exit_scope(&mut self) {
        let parent = self
            .state
            .borrow()
            .parent
            .clone()
            .expect("cannot exit global scope");

        self.state = parent;
    }

    // TODO: Move outside
    fn check_numbers(&self, left: &Eval<'de>, right: &Eval<'de>) -> bool {
        matches!(left, Eval::Number(_)) && matches!(right, Eval::Number(_))
    }

    fn check_strings(&self, left: &Eval<'de>, right: &Eval<'de>) -> bool {
        matches!(left, Eval::String(_)) && matches!(right, Eval::String(_))
    }

    fn check_booleans(&self, left: &Eval<'de>, right: &Eval<'de>) -> bool {
        matches!(left, Eval::Boolean(_)) && matches!(right, Eval::Boolean(_))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Eval<'de> {
    String(Cow<'de, str>),
    Number(f64),
    Boolean(bool),
    Ident(Cow<'de, str>),
    Print(Box<Eval<'de>>),
    Return(Box<Eval<'de>>),
    Error((String, i32)),
    Block(Vec<Eval<'de>>),
    Function {
        func_name: Cow<'de, str>,
        args: Vec<Eval<'de>>,
        block: &'de Box<Ast<'de>>,
    },
    Nil,
}

impl<'de> Eval<'de> {
    pub fn create_error(msg: impl AsRef<str>, code: i32) -> Self {
        Self::Error((msg.as_ref().to_string(), code))
    }
}

impl<'de> std::fmt::Display for Eval<'de> {
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
            Self::Return(ret) => write!(f, "return {ret}"),
            Self::Error((msg, code)) => write!(f, "{msg}: {code}"),
            Self::Block(statements) => {
                for statement in statements {
                    write!(f, "{statement} ")?;
                }
                Ok(())
            }
            Self::Function {
                func_name,
                args: _,
                block: _,
            } => write!(f, "<fn {func_name}>"),
        }
    }
}

#[derive(Debug)]
pub struct Scope<'de> {
    assignments: HashMap<Cow<'de, str>, Eval<'de>>,
    parent: Option<Rc<RefCell<Scope<'de>>>>,
}

impl<'de> Scope<'de> {
    pub fn new() -> Self {
        Self {
            assignments: HashMap::default(),
            parent: None,
        }
    }

    pub fn define(&mut self, name: impl AsRef<str>, value: Eval<'de>) {
        self.assignments
            .insert(name.as_ref().to_owned().into(), value);
    }

    pub fn assign(&mut self, name: impl AsRef<str>, value: Eval<'de>) -> bool {
        if self.assignments.contains_key(name.as_ref()) {
            self.assignments
                .insert(name.as_ref().to_owned().into(), value);
            return true;
        }

        if let Some(parent) = &self.parent {
            return parent.borrow_mut().assign(name, value);
        }

        false
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<Eval<'de>> {
        if let Some(value) = self.assignments.get(name.as_ref()) {
            return Some(value.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.borrow().get(name);
        }

        None
    }
}
