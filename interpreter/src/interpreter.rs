use std::{
    fs,
    io::{self, Write},
    path::Path,
};

use anyhow::{anyhow, Context};
use lexer::LoxLexer;

#[derive(Debug)]
pub struct LoxInterpreter {}

impl LoxInterpreter {
    pub fn new() -> LoxInterpreter {
        LoxInterpreter {}
    }

    pub fn run(&mut self, source: &str) -> anyhow::Result<()> {
        let lexer = LoxLexer::new();

        let tokens = lexer.lex_into_tokens(source).context("lexing failed")?;

        println!("lexed tokens: {tokens:?}");

        Ok(())
    }

    pub fn run_file(&mut self, file: &Path) -> anyhow::Result<()> {
        let source = fs::read_to_string(file)
            .with_context(|| format!("could not read file '{}'", file.display()))?;
        self.run(&source)
    }

    pub fn run_prompt(&mut self) -> anyhow::Result<()> {
        let mut line = String::new();

        loop {
            // show the prompt only if stdin is interactive
            if atty::is(atty::Stream::Stdin) {
                print!("> ");
                io::stdout().flush().context("flushing stdout failed")?;
            }

            match io::stdin().read_line(&mut line) {
                Ok(0) => return Ok(()), // EOF
                Ok(_) => self.run(&line)?,
                Err(e) => return Err(anyhow!(e).context("error while reading from stdin")),
            }
        }
    }
}
