# Alpacc <sup><sub><sup><sub>(Array Language Parallelism-Accelerated Compiler Compiler)</sup></sub></sup></sub>
Alpacc is a parallel LL parser generator which creates parsers written
in [Futhark](https://futhark-lang.org/). These parsers uses the LLP
grammmar class [1] together with a parallel lexer generator [2].

The parsing is done using the entry point `parse`. This function takes
a UTF-8 encoded array of `u8` values and returns some if it could
parse the input otherwise none. If the string could be parsed then an
nonempty array with a tree will be returned. The tree will be a
preorder traversal of the syntax tree where each node either is a
production or a terminal. If the node is a terminal then it will also
have value which gives the span of that token in the input
string. Each node will have a index to the parent node.

## Defining parsers
Either a terminal or a nonterminal can be defined on each line which
ends in a semicolon.

## Terminals
Terminals must be named with `lowercase` letters and are defined by a
regular expression. The valid regex operations are concatenation `RS`,
alternation `R|S`, kleene star `R*`, ranges `[a-zA-Z\s]` and one or
more matches `R+`.

Terminals can also be defined in the nonterminal definitions as a
string literal `"("`.

A special terminal is `ignore` which is a terminal that will always be
removed before the terminals are given to the parser or the terminals
are returned.

## Nonterminals
Nonterminals must be named with `UPPERCASE` letters by the possible
right-hand productions. A production can result in different
right-hand side separated by `|`. These right-hand sides are sequences
of nonterminals and terminals separated by whitespace. The first
production defined is the starting production for the grammar.

## Example
The following grammar is found in [grammars/paper_grammar.alp](grammars/paper_grammar.alp).
```
a = [0-9]+;
ignore = \s|\n|\t|\r;

E = T E';
E' = "+" T E' | ;
T = a | "[" E "]" ;
```
To construct an LLP(1, 1) grammar from this file the following command can be used.
```
cabal run -- alpacc futhark grammars/paper_grammar.alp -q 1 -k 1
```
This will create the Futhark source file `paper_grammar.fut` which contains the actual parser which is a function `parse`. 

A leftmost derivable string from this grammar is `1+[2+3]`. When parsing this the resulting syntax tree is.
```
#some [ (0, #production 0)
      , (0, #production 3)
      , (1, #terminal 0 (0, 1))
      , (0, #production 1)
      , (3, #terminal 1 (1, 2))
      , (3, #production 4)
      , (5, #terminal 2 (2, 3))
      , (5, #production 0)
      , (7, #production 3)
      , (8, #terminal 0 (3, 4))
      , (7, #production 1)
      , (10, #terminal 1 (4, 5))
      , (10, #production 3)
      , (12, #terminal 0 (5, 6))
      , (10, #production 2)
      , (5, #terminal 3 (6, 7))
      , (3, #production 2)
      ]
```
First element in each tuple is the parent index of each node, and we define the root node to be its own parent. The second element in each tuple is the actual node. A node is either a production or a terminal, and they are enumerated seperately. They are also enumerated in the order they are defined (besides `ignore` it will be included). So if we look at the first element then its parent is itself and the node is a production `#production 0`. The first production defined is `E = T E'` which is the starting production and has been given the value `0`. If we look at the element `(1, #terminal 0 (0, 1))` then its parent is at index `1` and its the terminal is `a`. `(0, 1))` is the span of the substring that token corresponds to in the input string.

It is also possible to only generate a lexer or parser using the flags
`--lexer` or `--parser`.

## Sources:
- [1] Ladislav Vagner and Bořivoj Melichar. 2007. Parallel LL
  parsing. Acta informatica 44, 1 (2007), 1–21.
- [2] Hill, J.M.D., Parallel lexical analysis and parsing on the AMT
  distributed array processor, Parallel Computing
- [3] Mogensen, T. Æ. (2017). Introduction to Compiler
Design. Undergraduate Topics in Computer
Science. https://doi.org/10.1007/978-3-319-66966-3 18 (1992) 699-714.
