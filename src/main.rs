use std::io;

mod error;
mod lexer;
mod parser;
mod position;
mod settings;

pub use error::{LoxError, LoxResult};
pub use lexer::{tokenize, tokenize_file, Token};
pub use position::{tag_position, FilePos, Position, PositionTagged};

fn print_error(error: LoxError<'_>) {
    println!("Error: {}", error);
}

fn run_interactive() {
    loop {
        let mut line = String::new();
        if io::stdin().read_line(&mut line).is_err() {
            break;
        }

        println!("{:#?}", tokenize(&line, "interactive", 1));
    }
}

fn run_file(input_file: &str) -> LoxResult<'_, ()> {
    println!(
        "Tokens: {:#?}",
        lexer::tokenize_file(input_file)?
    );

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
