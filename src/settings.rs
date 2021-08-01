use anyhow::Result;
use clap::{App, Arg};

#[derive(Debug)]
pub struct Settings {
    pub input_files: Vec<String>,
}

impl Settings {
    pub fn parse_cmd_line() -> Result<Self> {
        Self::parse(std::env::args())
    }

    pub fn parse(args: impl Iterator<Item = String>) -> Result<Self> {
        let matches = App::new("Rox")
            .version("1.0")
            .about("Rox interpreter")
            .arg(Arg::from_usage(
                "<INPUT>              'Sets the input file to use'",
            ))
            .get_matches_from_safe(args)?;

        Ok(Self {
            input_files: matches
                .values_of("INPUT")
                .unwrap()
                .map(|s| s.to_string())
                .collect(),
        })
    }
}
