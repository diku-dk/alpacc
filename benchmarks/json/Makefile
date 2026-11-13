H=\x1B[32m\033[1m
E=\033[0m\x1B[0m
D=\x1B[2m
GRAMMAR_NAME=$(notdir $(basename $(GRAMMAR)))
FUTHARK_BACKEND?=cuda
# INPUT_SIZE is a token count shared by lexer and parser benchmarks
# (same dataset, so identical payload bytes ≈ 2x the token count).
INPUT_SIZE?=52428800
GRAMMAR=../../grammars/json.alp
DATA=data-$(INPUT_SIZE).inputs
DATA_RAW=data-raw-$(INPUT_SIZE).json
NVCCFLAGS=-O3 -std=c++17 -arch=native

.PHONY: clean reset build bench bench-lexer bench-parser \
        bench-lexer-cuda bench-lexer-futhark bench-lexer-rust \
        bench-parser-cuda bench-parser-futhark \
        build-cuda profile-lexer-cuda \
        lexer.cu parser.cu

# --- Datasets ---

$(DATA): $(GRAMMAR)
	@printf "$(D)  Generating dataset [$(GRAMMAR_NAME), $(INPUT_SIZE) tokens]...$(E)\n"
	@alpacc test generate $(GRAMMAR) --single-long --no-outputs --length $(INPUT_SIZE) -o data-$(INPUT_SIZE) >/dev/null 2>&1

$(DATA_RAW): $(DATA)
	@dd if=$(DATA) of=$(DATA_RAW) bs=24 skip=1 >/dev/null 2>&1

# --- Lexer implementations ---

# Always regenerate: .cu files change when the compiler changes.
.PHONY: lexer.cu
lexer.cu:
	@printf "$(D)  Generating JSON lexer...$(E)\n"
	@alpacc cuda $(GRAMMAR) --lexer --index32 -o $@ >/dev/null 2>&1

lexer-cuda: lexer.cu
	@printf "$(D)  Compiling JSON CUDA lexer...$(E)\n"
	@nvcc $(NVCCFLAGS) -o $@ $^

lexer.fut: $(GRAMMAR) $(DATA_RAW)
	@printf "$(D)  Generating JSON Futhark lexer...$(E)\n"
	@alpacc futhark $(GRAMMAR) --lexer --index32 -o $@ >/dev/null 2>&1
	@printf -- "\n-- ==\n-- entry: lex_int\n-- script input { \$$loadbytes \"$(DATA_RAW)\" }\n" >> $@
	@futhark pkg sync > /dev/null 2>&1

lexer-rust: lexer.rs Cargo.toml Cargo.lock
	@printf "$(D)  Compiling JSON Rust lexer...$(E)\n"
	@cargo build --release >/dev/null 2>&1
	@cp target/release/lexer-rust .

# --- Parser implementations ---

# Always regenerate: .cu files change when the compiler changes.
.PHONY: parser.cu
parser.cu:
	@printf "$(D)  Generating JSON parser...$(E)\n"
	@alpacc cuda $(GRAMMAR) --index32 -o $@ >/dev/null 2>&1

parser-cuda: parser.cu
	@printf "$(D)  Compiling JSON CUDA parser...$(E)\n"
	@nvcc $(NVCCFLAGS) -o $@ $^

parser.fut: $(GRAMMAR) $(DATA_RAW)
	@printf "$(D)  Generating JSON Futhark parser...$(E)\n"
	@alpacc futhark $(GRAMMAR) --index32 -o $@ >/dev/null 2>&1
	@printf -- "\n-- ==\n-- entry: parse_int\n-- script input { \$$loadbytes \"$(DATA_RAW)\" }\n" >> $@
	@futhark pkg sync > /dev/null 2>&1

# --- Benchmarks ---

build: $(DATA) $(DATA_RAW) lexer-cuda parser-cuda lexer.fut parser.fut
	@printf "$(H)Build complete [$(GRAMMAR_NAME)].$(E)\n"

build-cuda: lexer.cu parser.cu
	@printf "$(D)  Compiling JSON CUDA lexer...$(E)\n"
	@nvcc $(NVCCFLAGS) -o lexer-cuda lexer.cu
	@printf "$(D)  Compiling JSON CUDA parser...$(E)\n"
	@nvcc $(NVCCFLAGS) -o parser-cuda parser.cu
	@printf "$(H)Build complete [$(GRAMMAR_NAME)].$(E)\n"

bench-lexer-cuda: $(DATA) lexer-cuda
	@printf "$(H)Benchmarking CUDA lexer [$(GRAMMAR_NAME)]:$(E)\n"
	@./lexer-cuda --benchmark 10 --warmup 3 < $(DATA)

bench-lexer-futhark: lexer.fut $(DATA_RAW)
	@futhark pkg sync > /dev/null 2>&1
	@printf "$(H)Benchmarking Futhark lexer [$(GRAMMAR_NAME)]:$(E)\n"
	@futhark bench --backend $(FUTHARK_BACKEND) lexer.fut

bench-lexer-rust: lexer-rust $(DATA_RAW)
	@printf "$(H)Benchmarking Rust lexer [$(GRAMMAR_NAME)]:$(E)\n"
	@./lexer-rust $(DATA_RAW)

bench-lexer:
	@printf "$(H)JSON Lexer Benchmark [$(GRAMMAR_NAME)]:$(E)\n"
	@$(MAKE) --no-print-directory bench-lexer-cuda bench-lexer-futhark

bench-parser-cuda: $(DATA) parser-cuda
	@printf "$(H)Benchmarking CUDA parser [$(GRAMMAR_NAME)]:$(E)\n"
	@./parser-cuda --benchmark 10 --warmup 3 < $(DATA)

bench-parser-futhark: parser.fut $(DATA_RAW)
	@futhark pkg sync > /dev/null 2>&1
	@printf "$(H)Benchmarking Futhark parser [$(GRAMMAR_NAME)]:$(E)\n"
	@futhark bench --backend $(FUTHARK_BACKEND) parser.fut

bench-parser:
	@printf "$(H)JSON Parser Benchmark [$(GRAMMAR_NAME)]:$(E)\n"
	@$(MAKE) --no-print-directory bench-parser-cuda bench-parser-futhark

bench: bench-lexer bench-parser

# --- Profiling ---

# Full ncu profile of one lexer kernel launch -> $(PROFILE_OUT).ncu-rep.
# Built with -lineinfo and the source imported, so ncu-ui attributes
# metrics to source lines.  --clock-control none profiles at the clocks the
# benchmark runs at (ncu otherwise locks them to base clocks).
NCU?=ncu
PROFILE_OUT?=lexer-cuda

lexer-cuda-profile: lexer.cu
	@printf "$(D)  Compiling JSON CUDA lexer (-lineinfo)...$(E)\n"
	@nvcc $(NVCCFLAGS) -lineinfo -o $@ $^

profile-lexer-cuda: $(DATA) lexer-cuda-profile
	@printf "$(H)Profiling CUDA lexer [$(GRAMMAR_NAME)] -> $(PROFILE_OUT).ncu-rep:$(E)\n"
	@$(NCU) --set full --import-source yes --clock-control none -k lexer -c 1 -f \
	        -o $(PROFILE_OUT) ./lexer-cuda-profile --benchmark 1 --warmup 1 < $(DATA)

reset:
	@rm -rf target lexer-rust lexer-cuda lexer-cuda-profile lexer.cu lexer.fut lexer lexer.c \
	       parser-cuda parser.cu parser.fut parser parser.c

clean: reset
	@rm -rf data-*.inputs data-raw-*.json lib
