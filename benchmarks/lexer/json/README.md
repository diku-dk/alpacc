# Benchmark
This folder contains LISP lexer benchmarks. The benchmarks are done by
trying to do the lexing of 100MiBs file of random tokens for LISP. The
token patters are defined as.
```
0. Whitespace: [\s\r\n\t]+
1. Token: [a-zA-Z0-9]+
2. Left parentheses: (
3. Right parentheses: )
```
The lexers will as they find a token return the index of the token
type and also the span of that token. None of the benchmarks account
for the time to read the input into memory or the time to create a
LISP lexer.

All benchmarks can be executed using.
```
$ make bench
```

## Input generation
The input generation is done by using.
```
make data.in
```
This creates the data.in file which contains ascii text of random
tokens defined from before. This program was written by
Troels Henriksen (athas).

## Rust Lexer
This benchmark is written in Rust using the lexer generator library
Logos. They claim to have a fast lexer so they seem like a good choice
to compare with. This benchmark is meant to work much like the futhark
lexer so every token will be saved in memory. This ends up lessening
the performance, assuming you can use the tokens as you go. The
benchmark also has the use that it will create a file in the futhark
binary data format. This is used to check for the correctness of the
futhark lexer. The benchmark can be executed using.
```
$ make bench-rust
```
## C Lexer
This benchmark is a handwritten lexer by Troels Henriksen (athas).
It finds every token in the input and does not save the tokens
found.
```
$ make bench-c
```

## Futhark Lexer
This benchmark generates a lexer using the alpacc lexer generator. It
will compute all tokens. The benchmark can be executed using.
```
$ make bench-futhark
```
