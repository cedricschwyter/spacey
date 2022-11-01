# spacey
a tiny, wasm-ready whitespace interpreter/virtual machine in rust

## project roadmap
- [x] whitespace parser
- [x] whitespace virtual machine
- [x] usable exposed crate API
- [x] spacey executable
- [ ] LLVM backend to compile to standalone executable
- [ ] termion/ncurses frontend with debugging capabilities
- [ ] make IO faster, get rid of `.clone()`'s in `Interpreter::next_instruction()`

## building and running
make sure you have rust and the rust package manager `cargo` installed on your system. if not, the easiest way to do so is to install rust via [rustup.rs](https://rustup.rs).

then, clone the repository and build natively as follows:

```bash
git clone https://github.com/D3PSI/spacey.git
cd spacey
cargo build --release
```

to build for wasm, simply install [wasm-pack](https://rustwasm.github.io/wasm-pack/installer/) and execute:

```bash
wasm-pack build
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

\*data extrapolated from single-instruction benchmark (`cargo bench`), which executes a single `PushStack` followed by an `Exit` instruction. benchmark run on a single Intel i7-7700K CPU core clocked at 5.1 GHz. this measurement is statistically significant because it was made with the `criterion.rs` statistical benchmarking suite. for comparison, the reference interpreter in Haskell manages 94.5 million instructions per second, according to [this](https://github.com/CensoredUsername/whitespace-rs). according to the same project, a JIT implementation beats spacey by about 200 million instructions per second, i.e., is a respectable (but only, considering JIT) around 25% faster than spacey. with some work I am confident I could match those numbers
