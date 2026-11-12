FUTHARK_BACKEND?=cuda
# Token count shared by lexer and parser benchmarks; payload bytes ≈ 2x.
INPUT_SIZE?=52428800

LANGS=json lisp
MAKEARGS=--no-print-directory FUTHARK_BACKEND=$(FUTHARK_BACKEND) INPUT_SIZE=$(INPUT_SIZE)

.PHONY: bench build clean reset bench-all-sizes \
        bench-json bench-lisp \
        bench-lexer bench-lexer-json bench-lexer-lisp \
        bench-parser bench-parser-json bench-parser-lisp \
        bench-cuda bench-futhark \
        build-json build-lisp clean-json clean-lisp reset-json reset-lisp

bench: bench-json bench-lisp

bench-json:
	@$(MAKE) -C json bench $(MAKEARGS)

bench-lisp:
	@$(MAKE) -C lisp bench $(MAKEARGS)

bench-lexer: bench-lexer-json bench-lexer-lisp

bench-lexer-json:
	@$(MAKE) -C json bench-lexer $(MAKEARGS)

bench-lexer-lisp:
	@$(MAKE) -C lisp bench-lexer $(MAKEARGS)

bench-parser: bench-parser-json bench-parser-lisp

bench-parser-json:
	@$(MAKE) -C json bench-parser $(MAKEARGS)

bench-parser-lisp:
	@$(MAKE) -C lisp bench-parser $(MAKEARGS)

bench-cuda:
	@printf '\n\033[1m=== JSON ===\033[0m\n'
	@$(MAKE) -C json build-cuda $(MAKEARGS)
	@$(MAKE) -C json bench-lexer-cuda bench-parser-cuda $(MAKEARGS)
	@printf '\n\033[1m=== Lisp ===\033[0m\n'
	@$(MAKE) -C lisp build-cuda $(MAKEARGS)
	@$(MAKE) -C lisp bench-lexer-cuda bench-parser-cuda $(MAKEARGS)
	@printf '\n\033[1m=== Done ===\033[0m\n'

bench-futhark:
	@printf '\n\033[1m=== JSON ===\033[0m\n'
	@$(MAKE) -C json bench-lexer-futhark bench-parser-futhark $(MAKEARGS)
	@printf '\n\033[1m=== Lisp ===\033[0m\n'
	@$(MAKE) -C lisp bench-lexer-futhark bench-parser-futhark $(MAKEARGS)
	@printf '\n\033[1m=== Done ===\033[0m\n'

bench-all-sizes:
	@$(MAKE) bench INPUT_SIZE=10485760 $(MAKEARGS)
	@$(MAKE) bench INPUT_SIZE=104857600 $(MAKEARGS)

build: build-json build-lisp

build-json:
	@$(MAKE) -C json build $(MAKEARGS)

build-lisp:
	@$(MAKE) -C lisp build $(MAKEARGS)

reset: reset-json reset-lisp

reset-json:
	@$(MAKE) -C json reset $(MAKEARGS)

reset-lisp:
	@$(MAKE) -C lisp reset $(MAKEARGS)

clean: clean-json clean-lisp

clean-json:
	@$(MAKE) -C json clean $(MAKEARGS)

clean-lisp:
	@$(MAKE) -C lisp clean $(MAKEARGS)
