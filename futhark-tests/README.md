# Tests
This folder contains tests to ensure the correctness of alpacc. The
tests are written in python and the tests files will be generated and
kept inside this folder. It is recommended to use `cabal exec` to run
the tests so you do not have to install alpacc everytime you want to
run the tests.
## Examples
Look inside the [workflows](/.github/workflows) folder to see how the
tests are used.

## Setup
You can set up the by passing the flag `--test-type=setup`. This will
download the futhark libraries and such.

## Stuck
This test can be done using the flags `--test-type=stuck`. This test
will generate a given number of grammars and see if alpacc gets stuck
while generating these grammars.

## Parsing
These tests can be generated using the flags `--test-type=parser`.
This test will generate a given number of parser for random grammars
and insert tests in the parsers to ensure that the derivations.

## Lexing
These tests can generated using the flags `--test-type=lexer`. You
simply have to define the tests inside the lexer-tests folder as a
json file using the same format as the other tests. These will check
that an existing lexer can parse the grammers the same way as the
alpacc lexers.
