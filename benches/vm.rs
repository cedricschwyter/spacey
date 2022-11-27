use criterion::{criterion_group, criterion_main, Criterion};
use spacey::{parser::SourceType, Vm, VmConfig, VmError};

pub fn single_instruction_with_param_benchmark(c: &mut Criterion) {
    let config =
        VmConfig::default_no_heap_suppressed("resources/push_stack.ws", SourceType::Whitespace);
    let mut vm = Vm::new(config).unwrap();
    c.bench_function("single instruction with param", |b| {
        b.iter(|| -> Result<(), VmError> {
            vm.run()?;

            Ok(())
        })
    });
}

pub fn reset_vm_benchmark(c: &mut Criterion) {
    let config =
        VmConfig::default_no_heap_suppressed("resources/hello_world.ws", SourceType::Whitespace);
    let mut vm = Vm::new(config).unwrap();
    c.bench_function("reset vm", |b| {
        b.iter(|| -> Result<(), VmError> {
            vm.reset();

            Ok(())
        })
    });
}

pub fn hello_world_benchmark(c: &mut Criterion) {
    let config =
        VmConfig::default_no_heap_suppressed("resources/hello_world.ws", SourceType::Whitespace);
    let mut vm = Vm::new(config).unwrap();
    c.bench_function("hello world", |b| {
        b.iter(|| -> Result<(), VmError> {
            vm.run()?;
            vm.reset();

            Ok(())
        })
    });
}

pub fn hello_world_of_spaces_benchmark(c: &mut Criterion) {
    let config = VmConfig::default_no_heap_suppressed(
        "resources/hello_world_of_spaces.ws",
        SourceType::Whitespace,
    );
    let mut vm = Vm::new(config).unwrap();
    c.bench_function("hello world of spaces", |b| {
        b.iter(|| -> Result<(), VmError> {
            vm.run()?;
            vm.reset();

            Ok(())
        })
    });
}

pub fn sieve_benchmark(c: &mut Criterion) {
    let config = VmConfig::default_no_heap_suppressed("resources/sieve.ws", SourceType::Whitespace);
    let mut vm = Vm::new(config).unwrap();
    c.bench_function("sieve", |b| {
        b.iter(|| -> Result<(), VmError> {
            vm.run()?;
            vm.reset();

            Ok(())
        })
    });
}

pub fn count_benchmark(c: &mut Criterion) {
    let config = VmConfig::default_no_heap_suppressed("resources/count.ws", SourceType::Whitespace);
    let mut vm = Vm::new(config).unwrap();
    c.bench_function("count", |b| {
        b.iter(|| -> Result<(), VmError> {
            vm.run()?;
            vm.reset();

            Ok(())
        })
    });
}

pub fn primes_benchmark(c: &mut Criterion) {
    let config =
        VmConfig::default_no_heap_suppressed("resources/primes.ws", SourceType::Whitespace);
    let mut vm = Vm::new(config).unwrap();
    c.bench_function("primes", |b| {
        b.iter(|| -> Result<(), VmError> {
            vm.run()?;
            vm.reset();

            Ok(())
        })
    });
}

criterion_group!(
    vm,
    count_benchmark,
    hello_world_benchmark,
    hello_world_of_spaces_benchmark,
    sieve_benchmark,
    reset_vm_benchmark,
    single_instruction_with_param_benchmark,
    primes_benchmark
);
criterion_main!(vm);
