use criterion::{criterion_group, criterion_main, Criterion};
use spacey::Interpreter;
use std::error::Error;

pub fn single_instruction_with_param_benchmark(c: &mut Criterion) {
    let mut interpreter =
        Interpreter::new("ws/push_stack.ws", 0, false, false, false, true).unwrap();
    c.bench_function("single instruction with param", |b| {
        b.iter(|| -> Result<(), Box<dyn Error>> {
            interpreter.run()?;

            Ok(())
        })
    });
}

pub fn reset_vm_benchmark(c: &mut Criterion) {
    let mut interpreter =
        Interpreter::new("ws/hello_world.ws", 0, false, false, false, true).unwrap();
    c.bench_function("reset vm", |b| {
        b.iter(|| -> Result<(), Box<dyn Error>> {
            interpreter.reset();

            Ok(())
        })
    });
}

pub fn hello_world_benchmark(c: &mut Criterion) {
    let mut interpreter =
        Interpreter::new("ws/hello_world.ws", 0, false, false, false, true).unwrap();
    c.bench_function("hello world", |b| {
        b.iter(|| -> Result<(), Box<dyn Error>> {
            interpreter.run()?;
            interpreter.reset();

            Ok(())
        })
    });
}

pub fn hello_world_of_spaces_benchmark(c: &mut Criterion) {
    let mut interpreter =
        Interpreter::new("ws/hello_world_of_spaces.ws", 0, false, false, false, true).unwrap();
    c.bench_function("hello world of spaces", |b| {
        b.iter(|| -> Result<(), Box<dyn Error>> {
            interpreter.run()?;
            interpreter.reset();

            Ok(())
        })
    });
}

pub fn sieve_benchmark(c: &mut Criterion) {
    let mut interpreter = Interpreter::new("ws/sieve.ws", 0, false, false, false, true).unwrap();
    c.bench_function("sieve", |b| {
        b.iter(|| -> Result<(), Box<dyn Error>> {
            interpreter.run()?;
            interpreter.reset();

            Ok(())
        })
    });
}

pub fn count_benchmark(c: &mut Criterion) {
    let mut interpreter = Interpreter::new("ws/count.ws", 0, false, false, false, true).unwrap();
    c.bench_function("count", |b| {
        b.iter(|| -> Result<(), Box<dyn Error>> {
            interpreter.run()?;
            interpreter.reset();

            Ok(())
        })
    });
}

criterion_group!(
    interpreter,
    count_benchmark,
    hello_world_benchmark,
    hello_world_of_spaces_benchmark,
    sieve_benchmark,
    reset_vm_benchmark,
    single_instruction_with_param_benchmark
);
criterion_main!(interpreter);
