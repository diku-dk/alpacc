H=\x1B[32m\033[1m
E=\033[0m\x1B[0m
D=\x1B[2m
GRAMMAR_NAME=$(notdir $(basename $(GRAMMAR)))
FUTHARK_BACKEND?=cuda
INPUT_SIZE?=52428800
GRAMMAR=../../grammars/numfmt.alp
DATA=data-$(INPUT_SIZE).inputs
NVCCFLAGS=-O3 -std=c++17 -arch=native

.PHONY: clean reset build bench bench-lexer bench-parser \
        bench-lexer-cuda bench-parser-cuda build-cuda \
        lexer.cu parser.cu

# --- Datasets ---

$(DATA): $(GRAMMAR)
	@printf "$(D)  Generating dataset [$(GRAMMAR_NAME), $(INPUT_SIZE) tokens]...$(E)\n"
	@alpacc test generate $(GRAMMAR) --single-long --no-outputs --length $(INPUT_SIZE) -o data-$(INPUT_SIZE) >/dev/null 2>&1

# --- Lexer implementations ---

.PHONY: lexer.cu
lexer.cu:
	@printf "$(D)  Generating numfmt lexer...$(E)\n"
	@alpacc cuda $(GRAMMAR) --lexer --index32 -o $@ >/dev/null 2>&1

lexer-cuda: lexer.cu
	@printf "$(D)  Compiling numfmt CUDA lexer...$(E)\n"
	@nvcc $(NVCCFLAGS) -o $@ $^

# --- Parser implementations ---

.PHONY: parser.cu
parser.cu:
	@printf "$(D)  Generating numfmt parser...$(E)\n"
	@alpacc cuda $(GRAMMAR) --index32 -o $@ >/dev/null 2>&1

parser-cuda: parser.cu
	@printf "$(D)  Compiling numfmt CUDA parser...$(E)\n"
	@nvcc $(NVCCFLAGS) -o $@ $^

# --- Benchmarks ---

build: $(DATA) lexer-cuda parser-cuda
	@printf "$(H)Build complete [$(GRAMMAR_NAME)].$(E)\n"

build-cuda: lexer.cu parser.cu
	@printf "$(D)  Compiling numfmt CUDA lexer...$(E)\n"
	@nvcc $(NVCCFLAGS) -o lexer-cuda lexer.cu
	@printf "$(D)  Compiling numfmt CUDA parser...$(E)\n"
	@nvcc $(NVCCFLAGS) -o parser-cuda parser.cu
	@printf "$(H)Build complete [$(GRAMMAR_NAME)].$(E)\n"

bench-lexer-cuda: $(DATA) lexer-cuda
	@printf "$(H)Benchmarking CUDA lexer [$(GRAMMAR_NAME)]:$(E)\n"
	@./lexer-cuda --benchmark 10 --warmup 3 < $(DATA)

bench-parser-cuda: $(DATA) parser-cuda
	@printf "$(H)Benchmarking CUDA parser [$(GRAMMAR_NAME)]:$(E)\n"
	@./parser-cuda --benchmark 10 --warmup 3 < $(DATA)

bench-lexer:
	@$(MAKE) --no-print-directory bench-lexer-cuda

bench-parser:
	@$(MAKE) --no-print-directory bench-parser-cuda

bench: bench-lexer bench-parser

reset:
	@rm -rf lexer-cuda lexer.cu parser-cuda parser.cu

clean: reset
	@rm -rf data-*.inputs
