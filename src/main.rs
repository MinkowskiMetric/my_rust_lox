use anyhow::Result;

mod error;
mod lexer;
mod position;
mod settings;

pub use error::{LoxError, LoxResult};
pub use position::{FilePos, Position, PositionTagged, tag_position};

fn main() -> Result<()> {
    let settings = settings::Settings::parse_cmd_line()?;
    println!("Input files: {:?}", settings);

    println!(
        "Tokens: {:?}",
        lexer::tokenize_file(&settings.input_files[0])
    );
    todo!()
}
