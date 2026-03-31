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
        while self.lexer.peek().is_some() {
            statements.push(self.parse_statement()?);
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
            // No arguments — the `)` will be consumed by the caller.
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
            statements.push(self.parse_statement()?);
        }
        self.lexer.expect(TokenType::RightBrace)?;

        Ok(Ast::Block(statements))
    }

    /// Optionally consume a trailing semicolon if one is present.
    fn consume_optional_semicolon(&mut self) {
        if matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                subtype: TokenType::Semicolon,
                ..
            }))
        ) {
            self.lexer.next();
        }
    }

    // ===========================================================================
    // Statement parsing
    // ===========================================================================
    //
    // Peek at the next token to determine which kind of statement we have,
    // then delegate to the appropriate sub-parser.  Semicolons are consumed
    // here (and only here) after the statement body is parsed.

    fn parse_statement(&mut self) -> Result<Ast<'de>, Error> {
        let peeked = match self.lexer.peek() {
            Some(Ok(token)) => token.subtype,
            Some(Err(_)) => {
                let err = self
                    .lexer
                    .next()
                    .expect("peeked Some")
                    .expect_err("peeked Err");
                return Err(err.into());
            }
            None => anyhow::bail!("unexpected end of input"),
        };

        let ast = match peeked {
            // Block statement: { ... }
            TokenType::LeftBrace => self.parse_block()?,

            // Print / return are prefix-style keyword statements whose
            // argument is a single expression.
            TokenType::Print | TokenType::Return => {
                let keyword = self.lexer.next().expect("peeked Some").expect("peeked Ok");
                let op = match keyword.subtype {
                    TokenType::Print => Op::Print,
                    TokenType::Return => Op::Return,
                    _ => unreachable!("by the match pattern above"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expression(r_bp)?;
                self.consume_optional_semicolon();
                Ast::Cons(op, vec![rhs])
            }

            TokenType::For => {
                self.lexer.next(); // consume `for`
                self.lexer.expect(TokenType::LeftParen)?;

                // Initialiser: either `;` (empty) or a full statement (which
                // consumes its own trailing `;`).
                let initialiser = if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        subtype: TokenType::Semicolon,
                        ..
                    }))
                ) {
                    self.lexer.next(); // consume bare `;`
                    Ast::Atom(Atom::Nil)
                } else {
                    self.parse_statement()?
                };

                // Condition: either `;` (empty → always true) or an expression
                // followed by `;`.
                let condition = if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        subtype: TokenType::Semicolon,
                        ..
                    }))
                ) {
                    self.lexer.next(); // consume bare `;`
                    Ast::Atom(Atom::Nil)
                } else {
                    let expr = self.parse_expression(0)?;
                    self.consume_optional_semicolon();
                    expr
                };

                // Increment: either `)` (empty) or an expression followed by `)`.
                let increment = if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        subtype: TokenType::RightParen,
                        ..
                    }))
                ) {
                    self.lexer.next(); // consume `)`
                    Ast::Atom(Atom::Nil)
                } else {
                    let expr = self.parse_expression(0)?;
                    self.lexer.expect(TokenType::RightParen)?;
                    expr
                };

                let block = match self.lexer.peek() {
                    Some(Ok(Token {
                        subtype: TokenType::LeftBrace,
                        ..
                    })) => self.parse_block()?,
                    Some(Ok(Token {
                        subtype: TokenType::Print,
                        ..
                    })) => self.parse_statement()?,
                    _ => self.parse_expression(0)?,
                };

                Ast::Cons(Op::For, vec![initialiser, condition, increment, block])
            }

            TokenType::While => {
                self.lexer.next(); // consume `while`
                self.lexer.expect(TokenType::LeftParen)?;
                let condition = self.parse_expression(0)?;
                self.lexer.expect(TokenType::RightParen)?;
                let block = if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        subtype: TokenType::LeftBrace,
                        ..
                    }))
                ) {
                    self.parse_block()?
                } else {
                    self.parse_statement()?
                };

                Ast::Cons(Op::While, vec![condition, block])
            }

            TokenType::Class => {
                self.lexer.next(); // consume `class`
                self.parse_class()?
            }

            TokenType::Fun => {
                self.lexer.next(); // consume `fun`
                self.parse_function()?
            }

            TokenType::Var => {
                self.lexer.next(); // consume `var`
                let token = self.lexer.expect(TokenType::Ident)?;
                let ident = Ast::Atom(Atom::Ident(token.source));
                match self.lexer.expect(TokenType::Equal) {
                    Ok(_) => {
                        let assignment = self.parse_expression(0)?;
                        self.consume_optional_semicolon();
                        Ast::Cons(Op::Var, vec![ident, assignment])
                    }
                    Err(_) => {
                        self.consume_optional_semicolon();
                        Ast::Cons(Op::Var, vec![ident, Ast::Atom(Atom::Nil)])
                    }
                }
            }

            TokenType::If => {
                self.lexer.next(); // consume `if`
                self.lexer.expect(TokenType::LeftParen)?;
                let condition = self.parse_expression(0)?;
                self.lexer.expect(TokenType::RightParen)?;
                let block = if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        subtype: TokenType::LeftBrace,
                        ..
                    }))
                ) {
                    self.parse_block()?
                } else {
                    self.parse_statement()?
                };
                let mut other = None;

                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        subtype: TokenType::Else,
                        ..
                    }))
                ) {
                    self.lexer.next(); // consume `else`
                    let block = if matches!(
                        self.lexer.peek(),
                        Some(Ok(Token {
                            subtype: TokenType::LeftBrace,
                            ..
                        }))
                    ) {
                        self.parse_block()?
                    } else {
                        self.parse_statement()?
                    };
                    other = Some(block);
                }

                Ast::If {
                    condition: Box::new(condition),
                    yes: Box::new(block),
                    no: other.map(Box::new),
                }
            }

            // Everything else is an expression statement.
            _ => {
                let expr = self.parse_expression(0)?;
                self.consume_optional_semicolon();
                expr
            }
        };

        Ok(ast)
    }

    // ===========================================================================
    // Expression parsing (Pratt parser)
    // ===========================================================================
    //
    // This is the *only* function that performs binding-power climbing.  It
    // never consumes semicolons — that is the caller's responsibility.

    fn parse_expression(&mut self, min_bp: u8) -> Result<Ast<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(Ast::Atom(Atom::Nil)),
            Some(Err(e)) => anyhow::bail!("expected expression - {e}"),
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
                let inner = self.parse_expression(0)?;
                self.lexer.expect(TokenType::RightParen)?;
                Ast::Cons(Op::Group, vec![inner])
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
            let op = match self.lexer.peek() {
                Some(Err(_)) => {
                    let err = self
                        .lexer
                        .next()
                        .expect("peeked Some")
                        .expect_err("peeked Err");
                    anyhow::bail!("expected operator - found {err:?}");
                }
                None => break,
                Some(Ok(token)) => match token.subtype {
                    // These tokens end an expression — let the caller handle them.
                    TokenType::RightParen
                    | TokenType::Comma
                    | TokenType::Semicolon
                    | TokenType::RightBrace => break,
                    TokenType::LeftParen => Op::Call,
                    _ => token.operation()?,
                },
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

        Ok(lhs)
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Print | Op::Return => ((), 1),
        Op::Minus | Op::Bang => ((), 15),
        _ => panic!("bad op: {op:?}"),
    }
}

fn infix_binding_power(op: Op) -> Option<(u8, u8)> {
    // Precedence (low → high):
    //   assignment < or < and < equality < comparison < addition < multiplication
    let res = match op {
        Op::Assign => (2, 1),
        Op::Or => (3, 4),
        Op::And => (5, 6),
        Op::BangEqual | Op::EqualEqual => (7, 8),
        Op::LessEqual | Op::Less | Op::GreaterEqual | Op::Greater => (9, 10),
        Op::Plus | Op::Minus => (11, 12),
        Op::Star | Op::Slash => (13, 14),
        Op::Field => (20, 19),
        _ => return None,
    };

    Some(res)
}

fn postfix_binding_power(op: Op) -> Option<(u8, ())> {
    match op {
        Op::Call => Some((17, ())),
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

impl<'de> std::fmt::Debug for Ast<'de> {
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
