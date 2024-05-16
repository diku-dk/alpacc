CC?=cc
CFLAGS=-O3
H=\x1B[32m\033[1m
E=\033[0m\x1B[0m
FUTHARK_BACKEND?=cuda
INPUT_SIZE?=104857600

.PHONY: clean bench build bench-rust bench-futhark bench-cuda

lexer-cuda: lexer.cu
	nvcc --std=c++14 -O3 -o $@ $^

data.json:
	@{ \
	if ! command -v mkjson &> /dev/null; \
	then \
		git clone https://github.com/mfussenegger/mkjson; \
		cd mkjson; \
		cabal install --overwrite-policy=always; \
	fi; \
	}

	mkjson >$@ 'data=replicate(10,object(when,randomDateTime(),what,randomDouble(0,10),status,randomInt(0,1)))'
lexer-rust: lexer.rs Cargo.toml Cargo.lock
	cargo build --release
	@cp target/release/lexer-rust .

json.fut: json.alp data.json
	alpacc json.alp --lexer
	@printf -- "\n-- ==\n-- entry: lex\n-- script input { \$$loadbytes \"data.json\" }\n-- output @ data.out\n" >> json.fut
	futhark pkg sync

build: lexer-rust lexer-c json.fut
	@:

data.out: lexer-rust data.json
	@printf "Creating output dataset...\n"
	@./lexer-rust data.json > /dev/null
	@printf "Done creating output dataset.\n"

bench-rust: lexer-rust data.json
	@printf "$(H)Benchmarking Rust implementation:$(E)\n"
	@./lexer-rust data.json

bench-c: lexer-c data.json
	@printf "$(H)Benchmarking C implementation:$(E)\n"
	@printf "Note: The result is not saved in memory.\n"
	@./lexer-c data.json

bench-cuda: lexer-cuda data.json
	@printf "$(H)Benchmarking CUDA implementation:$(E)\n"
	@./lexer-cuda data.json

bench-futhark: json.fut data.json data.out
	@printf "$(H)Benchmarking Futhark implementation:$(E)\n"
	futhark bench --backend $(FUTHARK_BACKEND) json.fut

bench: lexer-rust lexer-c lexer-cuda json.fut data.json
	@printf "$(H)Lisp Lexer Benchmark:$(E)\n"
	@printf "$(H)Rust implementation:$(E)\n"
	@./lexer-rust data.json

	@printf "$(H)C implementation:$(E)\n"
	@printf "Note: The result is not saved in memory.\n"
	@./lexer-c data.json

	@printf "$(H)Benchmarking CUDA implementation:$(E)\n"
	@./lexer-cuda data.json

	@printf "$(H)Futhark implementation:$(E)\n"
	futhark bench --backend $(FUTHARK_BACKEND) json.fut

clean:
	rm -rf tokens data.json target lexer-rust lexer-c data.out json json.c json.fut lexer-cuda
