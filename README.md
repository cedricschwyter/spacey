<div align="center" style="margin:10px;">
<image src="https://raw.githubusercontent.com/D3PSI/spacey/develop/resources/logo.png"></image>
</div>

# spacey
a tiny, performant, wasm-ready stack machine/virtual machine in rust

## supported and planned frontends:
- [x] whitespace
- [ ] malbolge
- [ ] brainfuck
- [ ] whatever other language we want to create a frontend for

## crates.io and npmjs.org
the library crate is available both as a standard crate [spacey](https://crates.io/crates/spacey) as well as a bundled wasm package on npm under [node-spacey](https://www.npmjs.com/package/node-spacey).

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
cargo run --release -q -- -f ws/hello_world.ws -t whitespace
```
