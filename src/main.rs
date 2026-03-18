#![allow(unused_variables)]
use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use codecrafters_interpreter::lex::{LexError, Lexer};
use std::path::PathBuf;

#[derive(Parser, Debug)]
struct Args {
    #[clap(subcommand)]
    command: LoxCommand,
}

#[derive(Subcommand, Debug)]
enum LoxCommand {
    Tokenize { filename: PathBuf },
}

fn main() -> Result<()> {
    let args = Args::parse();
    match args.command {
        LoxCommand::Tokenize { filename } => {
            let file_contents = std::fs::read_to_string(&filename)
                .with_context(|| format!("reading input file {}", filename.display()))?;

            for token in Lexer::new(&file_contents) {
                let token = match token {
                    Ok(t) => t,
                    Err(e) => {
                        match e {
                            LexError::InvalidToken(inner) => {
                                eprintln!(
                                    "[line {}] Error: Unexpected character: {}",
                                    inner.line(),
                                    inner.token
                                );
                            }
                            LexError::InvalidNumber(_) => {}
                            LexError::UnterminatedString(inner) => {
                                eprintln!("[line {}] Error: Unterminated string.", inner.line());
                            }
                        }
                        continue;
                    }
                };

                println!("{token}");
            }

            println!("EOF  null");
        }
    }

    Ok(())
}
