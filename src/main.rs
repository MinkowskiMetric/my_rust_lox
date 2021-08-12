use std::io;

mod bvalue;
mod callable;
mod class;
mod error;
mod instance;
mod interpreter;
mod lexer;
mod parser;
mod position;
mod resolver;
mod settings;
mod value;

pub use bvalue::{BValue, BValueType};
pub use callable::{Callable, NativeCallable, ScriptCallable};
pub use class::Class;
pub use error::{LoxError, LoxResult, UnwindableLoxError, UnwindableLoxResult};
pub use instance::{Instance, InstanceRef};
pub use interpreter::{interpret, EnvironmentRef, Interpreter};
pub use lexer::{tokenize, tokenize_file, PositionedToken, SimpleToken, Token};
pub use parser::{
    parse, BaseExpression, BinaryOp, Expression, ExpressionVisitor, LogicalBinaryOp, Parseable,
    Parser, ResolvedExpression, ResolvedIdentifier, ResolvedStatement, Statement, StatementVisitor,
    UnaryOp,
};
pub use position::{FilePos, Position};
pub use resolver::{resolve, resolve_statement, Resolvable};
pub use value::{Nil, Value};

fn print_error(error: LoxError) {
    println!("Error: {}", error);
}

fn run_interactive() -> LoxResult<()> {
    loop {
        let mut parse_data = String::new();
        let mut interpreter = Interpreter::new()?;
        let result = loop {
            let mut line = String::new();
            if io::stdin().read_line(&mut line).is_err() {
                break Ok(());
            }

            parse_data.push_str(&line);

            let statements: LoxResult<Vec<_>> = tokenize(&parse_data, "interactive", 1)
                .and_then(|tokens| tokens.parse().resolve().collect::<LoxResult<Vec<_>>>());

            match statements {
                Err(LoxError::IncompleteExpression(_)) => continue, // Get the next line
                Err(err) => {
                    break Err(err);
                }

                Ok(statements) => {
                    parse_data = String::new();

                    match interpreter.accept_statements(statements.iter()) {
                        Ok(_) => (),
                        Err(err) => {
                            break Err(err);
                        }
                    };
                }
            }
        };

        if let Err(err) = result {
            print_error(err);
        }
    }
}

fn run_file(input_file: &str) -> LoxResult<()> {
    lexer::tokenize_file(input_file)
        .and_then(|tokens| tokens.parse().resolve().collect())
        .and_then(|stmts: Vec<_>| interpret(&stmts))
}

fn main() {
    let settings = settings::Settings::parse_cmd_line();
    let result = if settings.interactive {
        run_interactive()
    } else {
        run_file(&settings.input_file)
    };

    if let Err(err) = result {
        print_error(err);
    }
}
