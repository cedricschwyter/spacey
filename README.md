# spacey
a tiny whitespace interpreter/virtual machine in rust

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
