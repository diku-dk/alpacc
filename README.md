# Alpacc
Alpacc is a LLP parser generator [1] which creates parsers written in [Futhark](https://futhark-lang.org/). The parser will be able to compute the sequence of production rules used to construct the input string.

## Defining parsers
Either a terminal or a nonterminal can be defined on each line which ends in a semicolon.

## Terminals
Terminals must be named with `lowercase` letters and are defined by a regular expression. The valid regex operations are concatenation `RS`, alternation `R|S`, kleene star `R*`, ranges `[a-zA-Z\s]` and one or more matches `R+`.

Terminals can also be defined in the nonterminal definitions as a string literal `"("`.

A special terminal is `ignore` which is a terminal that will always be removed before the terminals are given to the parser or the terminals are returned.

## Nonterminals
Nonterminals must be named with `UPPERCASE` letters by the possible right-hand productions. A production can result in different right-hand side separated by `|`. These right-hand sides are sequences of nonterminals and terminals separated by whitespace.

## Example
The following grammar is found in [grammars/paper_grammar.cg](grammars/paper_grammar.cg).
```
a = [0-9]+;
ignore = \s|\n|\t|\r;

E = T E';
E' = "+" T E' | ;
T = a | "[" E "]" ;
```
To construct an LLP(1, 1) grammar from this file the following command can be used.
```
cabal run alpacc -- grammars/paper_grammar.cg -q 1 -k 1
```
This will create the Futhark source file `paper_grammar.fut` which contains the actual parser which is a function `parse`. This function takes a string as input and maps this to an array of indexes assigned to the productions. The indexes are assigned in the order they are defined.

A leftmost derivable string from this grammar is `1+[2+3]`. When parsing this the resulting production sequence is `[0, 3, 1, 4, 0, 3, 1, 3, 2, 2]`.

If an input is given that cannot be parsed then the empty array is returned.

It is also possible to only generate a lexer or parser using the flags `--lexer` or `--parser`. 

## Sources:
- [1] Ladislav Vagner and Bořivoj Melichar. 2007. Parallel LL parsing. Acta informatica 44, 1 (2007), 1–21.
- [2] Mogensen, T. Æ. (2017). Introduction to Compiler Design. Undergraduate Topics in Computer Science. https://doi.org/10.1007/978-3-319-66966-3