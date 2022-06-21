use clap::{App, Arg, ArgMatches};
use spacey::Interpreter;
use std::error::Error;

fn args() -> ArgMatches {
    App::new("spacey")
        .about("a lightweight whitespace interpreter")
        .version("0.1.0")
        .author("Cedric Schwyter <cedricschwyter@bluewin.ch>")
        .arg(
            Arg::new("file")
                .short('f')
                .long("file")
                .takes_value(true)
                .required(true)
                .help("whitespace source file to interpret"),
        )
        .get_matches()
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = args();
    let file_name = args.value_of("file").unwrap();
    let mut interpreter = Interpreter::new(file_name)?;

    interpreter.run()?;

    Ok(())
}
