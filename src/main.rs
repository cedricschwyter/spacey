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
        .arg(
            Arg::new("heap-size")
                .short('s')
                .long("heap-size")
                .takes_value(true)
                .required(false)
                .help("the size of the heap address space (each heap address stores one i32)"),
        )
        .arg(
            Arg::new("ir")
                .short('i')
                .long("ir")
                .required(false)
                .takes_value(false)
                .help("prints intermediate representation of instructions"),
        )
        .arg(
            Arg::new("debug")
                .short('d')
                .long("debug")
                .takes_value(false)
                .required(false)
                .help("prints debug information after each executed instruction"),
        )
        .get_matches()
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = args();
    let file_name = args.value_of("file").unwrap();
    let heap_size = match args.value_of("heap-size") {
        Some(size) => size.parse()?,
        None => 524288,
    };
    let ir = args.is_present("ir");
    let debug = args.is_present("debug");
    let mut interpreter = Interpreter::new(file_name, heap_size, ir, debug)?;

    if !ir {
        interpreter.run()?;
    }

    Ok(())
}
