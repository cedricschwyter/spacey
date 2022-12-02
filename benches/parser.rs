use criterion::{criterion_group, criterion_main, Criterion};
use spacey::{parser::ParseError, WsParser};

pub fn parse_benchmark(c: &mut Criterion) {
    c.bench_function("parse", |b| {
        b.iter(|| -> Result<(), ParseError> {
            let mut parser = WsParser::new("resources/ws/quine.ws")?;
            parser.into_iter().for_each(|_instr| {});

            Ok(())
        })
    });
}

criterion_group!(parser, parse_benchmark,);
criterion_main!(parser);
