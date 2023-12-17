CC?=cc
CFLAGS=-O3
.PHONY: clean bench

tokens: tokens.c
	$(CC) -o $@ $^ $(CFLAGS)

data.in: tokens
	./$^ > data.in

lexer: main.rs Cargo.toml Cargo.lock
	cargo build --release
	cp target/release/lexer .

sexp.fut: sexp.cg data.in
	alpacc sexp.cg --lexer
	printf -- "\n-- ==\n-- entry: lex\n-- script input { \$$loadbytes \"data.in\" }\n" >> sexp.fut
	futhark pkg sync

bench: lexer data.in sexp.fut
	echo "Lisp Lexer Benchmark:"
	echo "Benchmarking Rust:"
	./$^
	echo "Benchmarking Futhark:"
	futhark bench --backend cuda sexp.fut

clean:
	rm -rf tokens data.in target lexer
