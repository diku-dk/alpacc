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
have value which gives the span of that token in the input string.
Each node will have a index to the parent node.

## Defining parsers
Either a terminal or a nonterminal can be defined on each line which
ends in a semicolon.

## Terminals
Terminals must be named with `lowercase` letters and are defined by a
regular expression. The valid regex operations are concatenation `RS`,
alternation `R|S`, kleene star `R*`, ranges `[a-z\xAF-\xFF]` and one
or more matches `R+`.

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
The following grammar is found in
[grammars/arithmetic.alp](grammars/arithmetic.alp).
```
params {
  lookback = 1;
  lookahead = 1;
};

num = /-?[0-9]+/;
var = /[a-z]+/;
ignore = /\s|\n|\t|\r/;

E0 -> E1 E0_;
E0_ [Add] -> "+" E1 E0_;
E0_ [Sub] -> "-" E1 E0_;
E0_ [Empty] -> ;

E1 -> E2 E1_;
E1_ [Mul] -> "*" E2 E1_;
E1_ [Div] -> "/" E2 E1_;
E1_ [Empty] -> ;

E2 [Parentheses] -> "(" E0 ")";
E2 [Num] -> num;
E2 [Var] -> var;
```

The grammar defines parameters which says the grammar needs a certain
amount of lookback and lookahead. And then three terminals are defined
and the productions are defined. These productions can be named using
the bracket notation but they can also not be named an a default name
will be given. To create the parser from this file the following
command can be used.

```
$ cabal run -- alpacc futhark grammars/arithmetic.alp
```

This will create the Futhark source file `arithmetic.fut` which
contains the actual parser which is a function `parse`.

An expression that can be parsed is `12+(3+4)` and when parsing this
results in the following concrete syntax tree.
```
#some [(0, #production (#E0_0)),
       (0, #production (#E1_4)), 
       (1, #production (#Num)), 
       (2, #terminal (#num) (0, 2)), 
       (1, #production (#Empty)), 
       (0, #production (#Add)), 
       (5, #terminal (#literal_5) (2, 3)), 
       (5, #production (#E1_4)), 
       (7, #production (#Parentheses)), 
       (8, #terminal (#literal_2) (3, 4)), 
       (8, #production (#E0_0)), 
       (10, #production (#E1_4)), 
       (11, #production (#Num)), 
       (12, #terminal (#num) (4, 5)), 
       (11, #production (#Mul)), 
       (14, #terminal (#literal_4) (5, 6)), 
       (14, #production (#Num)), 
       (16, #terminal (#num) (6, 7)), 
       (14, #production (#Empty)), 
       (10, #production (#Empty)), 
       (8, #terminal (#literal_3) (7, 8)), 
       (7, #production (#Empty)), 
       (5, #production (#Empty))]
```

First element in each tuple is the parent index of each node, where we
define the root node to be its own parent. The second element in each
tuple is the actual node. A node is either a production or a terminal,
and the names are either given by the name in the grammar otherwise
will `alpacc` try to give a reasonable name. So if we look at the
first element then its parent is itself and the node is a production
`#production (#E0_0)`. Which is the first production defined as `E0 ->
E1 E0_` which is the starting production. If we look at the terminal
`#terminal (#num) (0, 2)` then the terminal is `num` and `(0, 2)` is
the span of the substring that token corresponds to in the input
string.

It is also possible to only generate a lexer or parser using the flags
`--lexer` or `--parser`.

## Sources:
- [1] Ladislav Vagner and Bořivoj Melichar. 2007. Parallel LL
  parsing. Acta informatica 44, 1 (2007), 1–21.
- [2] Hill, J.M.D., Parallel lexical analysis and parsing on the AMT
  distributed array processor, Parallel Computing
- [3] Mogensen, T. Æ. (2017). Introduction to Compiler
  Design. Undergraduate Topics in Computer
  Science. https://doi.org/10.1007/978-3-319-66966-3 18 (1992)
  699-714.
