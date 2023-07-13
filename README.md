# Alpacc
Alpacc is a LLP(q, k) parser generator [1] which creates parsers written in [Futhark](https://futhark-lang.org/). The parser will be able to compute a sequence of production rules used to construct the input string.
### Example
The following grammar is found in [grammars/paper_grammar.cg](grammars/paper_grammar.cg).
```
a = [0-9]+;

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

### Ideas for future work:
 - [ ] Concrete syntax tree construction.
 - [ ] A lexer.
 - [ ] NumPy backend.

### Sources:
- [1] Ladislav Vagner and Bořivoj Melichar. 2007. Parallel LL parsing. Acta informatica 44, 1 (2007), 1–21.
- [2] Mogensen, T. Æ. (2017). Introduction to Compiler Design. Undergraduate Topics in Computer Science. https://doi.org/10.1007/978-3-319-66966-3