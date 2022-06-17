use clap::{App, Arg, ArgMatches};

enum LexicalTokenType {
    Space,
    Tab,
    Linefeed,
}

fn args() -> ArgMatches {
    App::new("spacey")
        .about("a whitespace interpreter")
        .version("0.1.0")
        .author("Cedric Schwyter <cedricschwyter@bluewin.ch>")
        .arg(
            Arg::new("file")
                .short('f')
                .long("file")
                .takes_value(true)
                .help("whitespace source file to interpret"),
        )
        .get_matches()
}

fn main() {
    let args = args();
    let file = args.value_of("file").unwrap();
}
