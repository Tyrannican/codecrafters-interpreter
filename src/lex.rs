use thiserror::Error;

#[derive(Debug, Clone)]
pub struct TokenError {
    pub src: String,
    pub offset: usize,
    pub token: char,
}

impl TokenError {
    pub fn line(&self) -> usize {
        let until_err = &self.src[..self.offset];
        until_err.lines().count()
    }
}

#[derive(Debug, Clone, Error)]
pub enum LexError {
    #[error("unexpected token")]
    InvalidToken(TokenError),

    #[error("unterminated string")]
    UnterminatedString(TokenError),

    #[error("invalid number")]
    InvalidNumber(TokenError),
}

#[derive(Debug)]
pub struct Lexer<'de> {
    input: &'de str,
    rest: &'de str,
    byte: usize,
    peeked: Option<Result<Token<'de>, LexError>>,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            input,
            rest: input,
            byte: 0,
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'de>, LexError>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }

        self.peeked = self.next();
        self.peeked.as_ref()
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.peeked.take() {
            return Some(next);
        }
        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let index = self.byte; // c_at
            let selected = &self.rest[..c.len_utf8()]; // c_str
            let remainder = self.rest; // c_onwards
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            let token = move |subtype: TokenType| {
                Some(Ok(Token {
                    subtype,
                    offset: index,
                    source: selected,
                }))
            };

            match c {
                '(' => return token(TokenType::LeftParen),
                ')' => return token(TokenType::RightParen),
                '{' => return token(TokenType::LeftBrace),
                '}' => return token(TokenType::RightBrace),
                ',' => return token(TokenType::Comma),
                '.' => return token(TokenType::Dot),
                '-' => return token(TokenType::Minus),
                '+' => return token(TokenType::Plus),
                ';' => return token(TokenType::Semicolon),
                '*' => return token(TokenType::Star),
                '/' => {
                    if self.rest.starts_with('/') {
                        let end = self.rest.find('\n').unwrap_or_else(|| self.rest.len());
                        self.byte += end;
                        self.rest = &self.rest[end..];
                        continue;
                    } else {
                        return Some(Ok(Token {
                            source: selected,
                            offset: index,
                            subtype: TokenType::Slash,
                        }));
                    }
                }
                '"' => {
                    if let Some(end) = self.rest.find('"') {
                        let literal = &remainder[..end + 1 + 1];
                        self.byte += end + 1;
                        self.rest = &self.rest[end + 1..];
                        return Some(Ok(Token {
                            source: literal,
                            offset: index,
                            subtype: TokenType::String,
                        }));
                    } else {
                        let err = TokenError {
                            src: self.input.to_string(),
                            offset: self.input.len() - (self.byte - c.len_utf8()),
                            token: '"',
                        };

                        self.byte += self.rest.len();
                        self.rest = &self.rest[self.rest.len()..];
                        return Some(Err(LexError::UnterminatedString(err)));
                    }
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let first_non_ident = remainder
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
                        .unwrap_or_else(|| remainder.len());
                    let literal = &remainder[..first_non_ident];
                    let extra = literal.len() - c.len_utf8();
                    self.byte += extra;
                    self.rest = &self.rest[extra..];

                    let subtype = match literal {
                        "and" => TokenType::And,
                        "class" => TokenType::Class,
                        "else" => TokenType::Else,
                        "false" => TokenType::False,
                        "for" => TokenType::For,
                        "fun" => TokenType::Fun,
                        "if" => TokenType::If,
                        "nil" => TokenType::Nil,
                        "or" => TokenType::Or,
                        "print" => TokenType::Print,
                        "return" => TokenType::Return,
                        "super" => TokenType::Super,
                        "this" => TokenType::This,
                        "true" => TokenType::True,
                        "var" => TokenType::Var,
                        "while" => TokenType::While,
                        _ => TokenType::Ident,
                    };

                    return Some(Ok(Token {
                        source: literal,
                        offset: index,
                        subtype,
                    }));
                }
                '0'..='9' => {
                    let first_non_digit = remainder
                        .find(|c| !matches!(c, '.' | '0'..='9'))
                        .unwrap_or_else(|| remainder.len());

                    let mut literal = &remainder[..first_non_digit];
                    let mut dotted = literal.splitn(3, '.');

                    match (dotted.next(), dotted.next(), dotted.next()) {
                        (Some(first), Some(second), Some(_)) => {
                            literal = &literal[..first.len() + 1 + second.len()];
                        }
                        (Some(first), Some(second), None) if second.is_empty() => {
                            literal = &literal[..first.len()];
                        }
                        _ => {}
                    }

                    let extra = literal.len() - c.len_utf8();
                    self.byte += extra;
                    self.rest = &self.rest[extra..];

                    let n = match literal.parse() {
                        Ok(n) => n,
                        Err(_) => {
                            let err = TokenError {
                                src: self.input.to_string(),
                                offset: self.byte - (self.byte - c.len_utf8()),
                                token: c,
                            };

                            return Some(Err(LexError::InvalidNumber(err)));
                        }
                    };

                    return Some(Ok(Token {
                        source: literal,
                        offset: index,
                        subtype: TokenType::Number(n),
                    }));
                }
                '<' | '>' | '!' | '=' => {
                    self.rest = self.rest.trim_start();
                    let trimmed = remainder.len() - self.rest.len() - 1;
                    self.byte += trimmed;

                    let (subtype, src) = if self.rest.starts_with('=') {
                        let ty = match c {
                            '<' => TokenType::LessEqual,
                            '>' => TokenType::GreaterEqual,
                            '!' => TokenType::BangEqual,
                            '=' => TokenType::EqualEqual,
                            _ => unreachable!("caught by above match"),
                        };

                        let src = &remainder[..c.len_utf8() + trimmed + 1];
                        self.rest = &self.rest[1..];
                        self.byte += 1;

                        (ty, src)
                    } else {
                        let ty = match c {
                            '<' => TokenType::Less,
                            '>' => TokenType::Greater,
                            '!' => TokenType::Bang,
                            '=' => TokenType::Equal,
                            _ => unreachable!("caught by above match"),
                        };

                        (ty, selected)
                    };

                    return Some(Ok(Token {
                        source: src,
                        offset: index,
                        subtype,
                    }));
                }
                c if c.is_whitespace() => continue,
                c => {
                    let err = TokenError {
                        src: self.input.to_string(),
                        token: c,
                        offset: self.byte,
                    };

                    return Some(Err(LexError::InvalidToken(err)));
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenClass<'de> {
    Atom(Atom<'de>),
    Op(Op),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Atom<'de> {
    String(&'de str),
    Number(f64),
    Nil,
    Bool(bool),
    Ident(&'de str),
    Super,
    This,
}

impl std::fmt::Display for Atom<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{}", s.trim_matches('"')),
            Self::Number(n) => {
                if *n == n.trunc() {
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
            Self::Nil => write!(f, "nil"),
            Self::Bool(b) => write!(f, "{b:?}"),
            Self::Ident(i) => write!(f, "{i}"),
            Self::Super => write!(f, "super"),
            Self::This => write!(f, "this"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Op {
    Minus,
    Plus,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Bang,
    And,
    Or,
    Call,
    For,
    Class,
    Print,
    Return,
    Field,
    Var,
    While,
    Group,
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::Star => write!(f, "*"),
            Self::BangEqual => write!(f, "!="),
            Self::EqualEqual => write!(f, "=="),
            Self::LessEqual => write!(f, "<="),
            Self::GreaterEqual => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::Greater => write!(f, ">"),
            Self::Slash => write!(f, "/"),
            Self::Bang => write!(f, "!"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Call => write!(f, "call"),
            Self::For => write!(f, "for"),
            Self::Class => write!(f, "class"),
            Self::Print => write!(f, "print"),
            Self::Return => write!(f, "return"),
            Self::Field => write!(f, "."),
            Self::Var => write!(f, "var"),
            Self::While => write!(f, "while"),
            Self::Group => write!(f, "group"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'de> {
    source: &'de str,
    offset: usize,
    pub subtype: TokenType,
}

impl<'de> Token<'de> {
    pub fn class(&self) -> Option<TokenClass<'de>> {
        match self.subtype {
            // Handled by the parser
            TokenType::RightParen
            | TokenType::LeftBrace
            | TokenType::RightBrace
            | TokenType::Comma
            | TokenType::Semicolon
            | TokenType::Equal
            | TokenType::If
            | TokenType::Else
            | TokenType::Fun => None,

            // Atoms
            TokenType::False => Some(TokenClass::Atom(Atom::Bool(false))),
            TokenType::True => Some(TokenClass::Atom(Atom::Bool(true))),
            TokenType::String => Some(TokenClass::Atom(Atom::String(self.source))),
            TokenType::Number(n) => Some(TokenClass::Atom(Atom::Number(n))),
            TokenType::Nil => Some(TokenClass::Atom(Atom::Nil)),
            TokenType::Super => Some(TokenClass::Atom(Atom::Super)),
            TokenType::This => Some(TokenClass::Atom(Atom::This)),
            TokenType::Ident => Some(TokenClass::Atom(Atom::Ident(self.source))),

            // Operations
            TokenType::Star => Some(TokenClass::Op(Op::Star)),
            TokenType::Dot => Some(TokenClass::Op(Op::Field)),
            TokenType::Plus => Some(TokenClass::Op(Op::Plus)),
            TokenType::Minus => Some(TokenClass::Op(Op::Minus)),
            TokenType::EqualEqual => Some(TokenClass::Op(Op::EqualEqual)),
            TokenType::Bang => Some(TokenClass::Op(Op::Bang)),
            TokenType::BangEqual => Some(TokenClass::Op(Op::BangEqual)),
            TokenType::Less => Some(TokenClass::Op(Op::Less)),
            TokenType::LessEqual => Some(TokenClass::Op(Op::LessEqual)),
            TokenType::Greater => Some(TokenClass::Op(Op::Greater)),
            TokenType::GreaterEqual => Some(TokenClass::Op(Op::GreaterEqual)),
            TokenType::Slash => Some(TokenClass::Op(Op::Slash)),
            TokenType::And => Some(TokenClass::Op(Op::And)),
            TokenType::Class => Some(TokenClass::Op(Op::Class)),
            TokenType::For => Some(TokenClass::Op(Op::For)),
            TokenType::Or => Some(TokenClass::Op(Op::Or)),
            TokenType::Print => Some(TokenClass::Op(Op::Print)),
            TokenType::Return => Some(TokenClass::Op(Op::Return)),
            TokenType::Var => Some(TokenClass::Op(Op::Var)),
            TokenType::While => Some(TokenClass::Op(Op::While)),
            TokenType::LeftParen => Some(TokenClass::Op(Op::Group)),
        }
    }
}

impl<'de> std::fmt::Display for Token<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source = self.source;
        match self.subtype {
            TokenType::LeftParen => write!(f, "LEFT_PAREN {source} null"),
            TokenType::RightParen => write!(f, "RIGHT_PAREN {source} null"),
            TokenType::LeftBrace => write!(f, "LEFT_BRACE {source} null"),
            TokenType::RightBrace => write!(f, "RIGHT_BRACE {source} null"),
            TokenType::Star => write!(f, "STAR {source} null"),
            TokenType::Dot => write!(f, "DOT {source} null"),
            TokenType::Comma => write!(f, "COMMA {source} null"),
            TokenType::Plus => write!(f, "PLUS {source} null"),
            TokenType::Minus => write!(f, "MINUS {source} null"),
            TokenType::Equal => write!(f, "EQUAL {source} null"),
            TokenType::EqualEqual => write!(f, "EQUAL_EQUAL {source} null"),
            TokenType::Bang => write!(f, "BANG {source} null"),
            TokenType::BangEqual => write!(f, "BANG_EQUAL {source} null"),
            TokenType::Less => write!(f, "LESS {source} null"),
            TokenType::LessEqual => write!(f, "LESS_EQUAL {source} null"),
            TokenType::Greater => write!(f, "GREATER {source} null"),
            TokenType::GreaterEqual => write!(f, "GREATER_EQUAL {source} null"),
            TokenType::Slash => write!(f, "SLASH {source} null"),
            TokenType::Semicolon => write!(f, "SEMICOLON {source} null"),
            TokenType::String => write!(f, "STRING {source} {}", self.source.trim_matches('"')),
            TokenType::Number(n) => {
                if n == n.trunc() {
                    write!(f, "NUMBER {source} {n}.0")
                } else {
                    write!(f, "NUMBER {source} {n}")
                }
            }
            TokenType::Ident => write!(f, "IDENTIFIER {source} null"),
            TokenType::And => write!(f, "AND and null"),
            TokenType::Class => write!(f, "CLASS class null"),
            TokenType::Else => write!(f, "ELSE else null"),
            TokenType::False => write!(f, "FALSE false null"),
            TokenType::For => write!(f, "FOR for null"),
            TokenType::Fun => write!(f, "FUN fun null"),
            TokenType::If => write!(f, "IF if null"),
            TokenType::Nil => write!(f, "NIL nil null"),
            TokenType::Or => write!(f, "OR or null"),
            TokenType::Print => write!(f, "PRINT print null"),
            TokenType::Return => write!(f, "RETURN return null"),
            TokenType::Super => write!(f, "SUPER super null"),
            TokenType::This => write!(f, "THIS this null"),
            TokenType::True => write!(f, "TRUE true null"),
            TokenType::Var => write!(f, "VAR var null"),
            TokenType::While => write!(f, "WHILE while null"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Star,
    Dot,
    Comma,
    Plus,
    Minus,
    Semicolon,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    String,
    Slash,
    Number(f64),
    Ident,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}
