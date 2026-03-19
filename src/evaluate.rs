use crate::parse::Ast;

use anyhow::Result;

pub struct Evaluator<'de> {
    ast: Ast<'de>,
}

impl<'de> Evaluator<'de> {
    pub fn new(ast: Ast<'de>) -> Self {
        Self { ast }
    }

    pub fn evaluate(&mut self) -> Result<()> {
        println!("EVALUATE");
        Ok(())
    }
}
