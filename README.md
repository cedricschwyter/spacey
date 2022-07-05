# spacey
a tiny whitespace interpreter/virtual machine in rust

## project roadmap
- [x] whitespace parser
- [x] whitespace virtual machine
- [x] usable exposed crate API
- [x] spacey executable
- [ ] LLVM backend to compile to standalone executable
- [ ] termion/ncurses frontend with debugging capabilities

## building and running
make sure you have rust and the rust package manager `cargo` installed on your system. if not, the easiest way to do so is to install rust via [rustup.rs](https://rustup.rs).

then, clone the repository and run as follows:

```bash
git clone https://github.com/D3PSI/spacey.git
cd spacey
cargo build --release
```

to run the provided executable run:

```bash
cargo run --release -q -- -h
```

to show the help screen.

execute whitespace-files like:

```bash
cargo run --release -q -- -f ws/hello_world.ws
```

*data extrapolated from single-instruction benchmark (`cargo bench`), which executes a single `PushStack` instruction
