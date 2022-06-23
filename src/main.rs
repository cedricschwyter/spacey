use clap::{App, Arg, ArgMatches};
use spacey::Interpreter;
use std::error::Error;
use std::time::Instant;

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
        .arg(
            Arg::new("debug-heap")
                .short('m')
                .long("debug-heap")
                .takes_value(false)
                .required(false)
                .help("prints a heap dump after each executed instruction"),
        )
        .arg(
            Arg::new("quiet")
                .short('q')
                .long("quiet")
                .required(false)
                .takes_value(false)
                .help("suppresses all output other than what the whitespace program is producing"),
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
    let debug_heap = args.is_present("debug-heap");
    let quiet = args.is_present("quiet");
    if !quiet {
        println!(
        "initializing, loading and parsing the provided source, creating the virtual machine..."
    );
    }
    let start = Instant::now();
    let mut interpreter = Interpreter::new(file_name, heap_size, ir, debug, debug_heap)?;
    let end = Instant::now();
    if !quiet {
        println!(
            "initialized in {} ms ({} ns)",
            end.duration_since(start).as_millis(),
            end.duration_since(start).as_nanos()
        );
    }

    if !ir {
        if !quiet {
            println!("starting to execute whitespace routine...\n\n");
        }
        let start = Instant::now();
        interpreter.run()?;
        let end = Instant::now();
        if !quiet {
            println!(
                "\n\n\nroutine took {} ms ({} ns)",
                end.duration_since(start).as_millis(),
                end.duration_since(start).as_nanos()
            );
        }
    }

    Ok(())
}
