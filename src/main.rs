use std::io;

mod bvalue;
mod callable;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod position;
mod settings;
mod value;

pub use bvalue::{BValue, BValueType};
pub use callable::Callable;
pub use error::{LoxError, LoxResult};
pub use interpreter::{interpret, Interpreter};
pub use lexer::{tokenize, tokenize_file, SimpleToken, Token};
pub use parser::{
    parse, BinaryOp, Expression, ExpressionVisitor, LogicalBinaryOp, Parser, Statement,
    StatementVisitor, UnaryOp,
};
pub use position::{tag_position, FilePos, Position, PositionTagged};
pub use value::{Nil, Value};

fn print_error(error: LoxError) {
    println!("Error: {}", error);
}

fn run_interactive() {
    loop {
        let mut parse_data = String::new();
        let mut interpreter = Interpreter::new();
        loop {
            let mut line = String::new();
            if io::stdin().read_line(&mut line).is_err() {
                break;
            }

            parse_data.push_str(&line);

            let statements: LoxResult<Vec<_>> =
                tokenize(&parse_data, "interactive", 1).and_then(|tokens| parse(tokens).collect());
            match statements {
                Err(LoxError::IncompleteExpression(_)) => continue, // Get the next line
                Err(err) => {
                    print_error(err);
                    break;
                }

                Ok(statements) => {
                    parse_data = String::new();

                    match interpreter.accept_statements(statements.iter()) {
                        Ok(_) => (),
                        Err(err) => {
                            print_error(err);
                            break;
                        }
                    };
                }
            }
        }
    }
}

fn run_file(input_file: &str) -> LoxResult<()> {
    lexer::tokenize_file(input_file)
        .and_then(|tokens| parse(tokens).collect())
        .and_then(|stmts: Vec<_>| interpret(&stmts))
}

fn main() {
    let settings = settings::Settings::parse_cmd_line();
    if settings.interactive {
        run_interactive();
    } else {
        if let Err(err) = run_file(&settings.input_file) {
            print_error(err);
        }
    };
}
