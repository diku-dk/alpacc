CC?=cc
CFLAGS=-O3
H=\x1B[32m\033[1m
E=\033[0m\x1B[0m
FUTHARK_BACKEND?=cuda
INPUT_SIZE?=104857600
.PHONY: clean bench build bench-rust bench-c bench-futhark

tokens: tokens.c
	$(CC) -o $@ $^ $(CFLAGS)

lexer-c: lexer.c
	$(CC) -o $@ $^ $(CFLAGS)

data.in: tokens
	@printf "Creating input dataset...\n"
	@./$^ $(INPUT_SIZE) > data.in
	@printf "Done creating input dataset.\n"

lexer-rust: lexer.rs Cargo.toml Cargo.lock
	cargo build --release
	@cp target/release/lexer-rust .

sexp.fut: sexp.alp data.in
	alpacc sexp.alp --lexer
	@printf -- "\n-- ==\n-- entry: lex\n-- script input { \$$loadbytes \"data.in\" }\n-- output @ data.out\n" >> sexp.fut
	futhark pkg sync

build: lexer-rust lexer-c sexp.fut
	@:

data.out: lexer-rust data.in
	@printf "Creating output dataset...\n"
	@./lexer-rust data.in > /dev/null
	@printf "Done creating output dataset.\n"

bench-rust: lexer-rust data.in
	@printf "$(H)Benchmarking Rust implementation:$(E)\n"
	@./lexer-rust data.in

bench-c: lexer-c data.in
	@printf "$(H)Benchmarking C implementation:$(E)\n"
	@printf "Note: The result is not saved in memory.\n"
	@./lexer-c data.in

bench-futhark: sexp.fut data.in data.out
	@printf "$(H)Benchmarking Futhark implementation:$(E)\n"
	futhark bench --backend $(FUTHARK_BACKEND) sexp.fut

bench: lexer-rust lexer-c sexp.fut data.in
	@printf "$(H)Lisp Lexer Benchmark:$(E)\n"
	@printf "$(H)Rust implementation:$(E)\n"
	@./lexer-rust data.in

	@printf "$(H)C implementation:$(E)\n"
	@printf "Note: The result is not saved in memory.\n"
	@./lexer-c data.in

	@printf "$(H)Futhark implementation:$(E)\n"
	futhark bench --backend $(FUTHARK_BACKEND) sexp.fut

clean:
	rm -rf tokens data.in target lexer-rust lexer-c data.out sexp sexp.c sexp.fut
