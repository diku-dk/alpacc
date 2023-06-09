# Parallel Parser
This is a LLP(q, k) parser generator [1] which creates parsers written in [Futhark](https://futhark-lang.org/). The parser will be able to compute a sequence of production rules used to construct the input string.
### Example
To use the parser generator a context-free grammar will first need to be defined. A context-free grammar is a quadruple $(S, T, N, P)$ where $S$ is the starting nonterminal, $T$ is the set of terminals, $N$ is the set of nonterminals and $P$ is the set of productions. An example of such a grammar [2, p. 41] is found in [grammars/book_grammar.cg](grammars/book_grammar.cg).
```
(
    T',
    {a, b, c, $},
    {T', T, R},
    {
        T' -> T $,
        T -> R,
        T -> a T c,
        R -> ,
        R -> b R
    }
)
```
To construct an LLP(1, 1) grammar from this file the following command can be used.
```
cabal run parallel-parser -- grammars/paper_grammar.cg -q 1 -k 1
```
This will create the Futhark source file `paper_grammar.fut` which contains the actual parser which is a function `parse`. This function takes as input an array of indexes assigned to terminals and maps this to an array of indexes assigned to the productions. The indexes are assigned in the order they are defined. For this example the indexes for the terminals would be `0` is `a`, `1` is `b`, `2` is `c` and `3` is `$`. For the productions `0` is `T' -> T $`, `1` is `T -> R`, `2` is `T -> a T c`, `3` is `R -> `, `4` is `R -> b R`.

A leftmost derivable string from this grammar is `aabbbcc$` which corresponds to the indexes `[0, 0, 1, 1, 1, 2, 2, 3]`. When parsing this array the resulting productions sequence is `[0, 2, 2, 1, 4, 4, 4, 3]`.

If an input is given that cannot be parsed then the empty array is returned.

### Escape Characters
Certain characters need to be escaped in the `.cg` file. The following substrings maps to the strings on the right.
- "\\\\": "\\"
- "\\,": ","
- "\\}": "}"
- "\\t": t"
- "\\r": r"
- "\\n": n"
- "\\s": " "
- "\\": ""

### Ideas for future work:
 - [ ] Concrete syntax tree construction.
 - [ ] A lexer.
 - [ ] NumPy backend.

### Sources:
- [1] Ladislav Vagner and Bořivoj Melichar. 2007. Parallel LL parsing. Acta informatica 44, 1 (2007), 1–21.
- [2] Mogensen, T. Æ. (2017). Introduction to Compiler Design. Undergraduate Topics in Computer Science. https://doi.org/10.1007/978-3-319-66966-3