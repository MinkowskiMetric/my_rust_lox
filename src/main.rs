use std::io;

mod bvalue;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod position;
mod settings;
mod value;

pub use bvalue::{BValue, BValueType};
pub use error::{LoxError, LoxResult};
pub use interpreter::interpret;
pub use lexer::{tokenize, tokenize_file, Token};
pub use parser::{parse, BinaryOp, Expression, ExpressionVisitor, Parser, UnaryOp};
pub use position::{tag_position, FilePos, Position, PositionTagged};
pub use value::{Nil, Value};

fn print_error(error: LoxError) {
    println!("Error: {}", error);
}

fn run_interactive() {
    loop {
        let mut parse_data = String::new();
        loop {
            let mut line = String::new();
            if io::stdin().read_line(&mut line).is_err() {
                break;
            }

            parse_data.push_str(&line);

            match tokenize(&parse_data, "interactive", 1).and_then(|tokens| parse(tokens)) {
                Ok(expr) => {
                    println!("EXPR: {}", expr.value());
                    println!("RESULT: {:?}", interpret(expr.value()));
                    break;
                }

                Err(LoxError::IncompleteExpression(_)) => continue, // Get the next line
                Err(err) => {
                    print_error(err);
                    break;
                }
            }
        }
    }
}

fn run_file(input_file: &str) -> LoxResult<()> {
    println!("Tokens: {:#?}", lexer::tokenize_file(input_file)?);

    Ok(())
}

fn main() {
    let settings = settings::Settings::parse_cmd_line();
    let result = if settings.interactive {
        run_interactive();
    } else {
        if let Err(err) = run_file(&settings.input_file) {
            print_error(err);
        }
    };
}
