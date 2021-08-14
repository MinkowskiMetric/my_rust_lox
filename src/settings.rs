use clap::{App, Arg};

#[derive(Debug)]
pub struct Settings {
    pub input_file: String,
    pub interactive: bool,
}

impl Settings {
    pub fn parse_cmd_line() -> Self {
        Self::parse(std::env::args())
    }

    pub fn parse(args: impl Iterator<Item = String>) -> Self {
        let matches = App::new("Rox")
            .version("1.0")
            .about("Rox interpreter")
            .arg(
                Arg::with_name("input")
                    .index(1)
                    .takes_value(true)
                    .required_unless("interactive")
                    .help("Input files"),
            )
            .arg(
                Arg::with_name("interactive")
                    .short("i")
                    .long("interactive")
                    .help("Interactive mode")
                    .conflicts_with("input"),
            )
            .get_matches_from(args);

        Self {
            input_file: matches
                .value_of("input")
                .map(String::from)
                .unwrap_or_else(String::new),
            interactive: matches.is_present("interactive"),
        }
    }
}
