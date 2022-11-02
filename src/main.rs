use clap::{App, Arg, ArgMatches};
use spacey::interpreter::interpreter::InterpretError;
use spacey::{Interpreter, InterpreterConfig};
use std::time::Instant;

const ARG_FILE: &str = "file";
const ARG_HEAP_SIZE: &str = "heap-size";
const ARG_RAW: &str = "raw";
const ARG_DEBUG: &str = "debug";
const ARG_DEBUG_HEAP: &str = "debug-file";
const ARG_QUIET: &str = "quiet";

fn args() -> ArgMatches {
    App::new("spacey")
        .about("a lightweight whitespace interpreter")
        .version("0.1.0")
        .author("Cedric Schwyter <cedricschwyter@bluewin.ch>")
        .arg(
            Arg::new(ARG_FILE)
                .short('f')
                .long(ARG_FILE)
                .takes_value(true)
                .required(true)
                .help("whitespace source file to interpret"),
        )
        .arg(
            Arg::new(ARG_HEAP_SIZE)
                .short('s')
                .long(ARG_HEAP_SIZE)
                .takes_value(true)
                .required(false)
                .help("the size of the heap address space (each heap address stores one i32)"),
        )
        .arg(
            Arg::new(ARG_RAW)
                .short('i')
                .long(ARG_RAW)
                .required(false)
                .takes_value(false)
                .help("prints intermediate representation of instructions"),
        )
        .arg(
            Arg::new(ARG_DEBUG)
                .short('d')
                .long(ARG_DEBUG)
                .takes_value(false)
                .required(false)
                .help("prints debug information after each executed instruction"),
        )
        .arg(
            Arg::new(ARG_DEBUG_HEAP)
                .short('m')
                .long(ARG_DEBUG_HEAP)
                .takes_value(false)
                .required(false)
                .help("prints a heap dump after each executed instruction"),
        )
        .arg(
            Arg::new(ARG_QUIET)
                .short('q')
                .long(ARG_QUIET)
                .required(false)
                .takes_value(false)
                .help("suppresses all output other than what the whitespace program is producing"),
        )
        .get_matches()
}

fn main() -> Result<(), InterpretError> {
    let args = args();
    let file_name = args.value_of(ARG_FILE).unwrap();
    let heap_size = match args.value_of(ARG_HEAP_SIZE) {
        Some(size) => size.parse().unwrap(),
        None => 524288,
    };
    let raw = args.is_present(ARG_RAW);
    let debug = args.is_present(ARG_DEBUG);
    let debug_heap = args.is_present(ARG_DEBUG_HEAP);
    let quiet = args.is_present(ARG_QUIET);
    if !quiet {
        println!(
        "initializing, loading and parsing the provided source, creating the virtual machine..."
    );
    }
    let start = Instant::now();
    let config = InterpreterConfig::new(file_name, heap_size, raw, debug, debug_heap, false);
    let mut interpreter = Interpreter::new(config)?;
    let end = Instant::now();
    if !quiet {
        println!(
            "initialized in {} ms ({} ns)",
            end.duration_since(start).as_millis(),
            end.duration_since(start).as_nanos()
        );
    }

    if !raw {
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
