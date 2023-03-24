# ParallelParser
This is an early stage LLP(q, k) table generator [1] which spits out a list of productions rules.
Currently it does not report parsing errors and $q$ and $k$ has to be manually set in app/Main.hs.
To use the parser generator do. 
```
    $ cat some_grammar.in | cabal run parallel-parser
```
This will print a parser written in [Futhark](https://futhark-lang.org/) where the `parse` function takes an array of indexes.
The indexes corresponds to the indexes of the terminals in the `.in` file where 0 is the starting index.
`parse` then returns a list of indexes that corresponds to the indexes of the productions.

To see examples of how to define a context-free grammar look in the `.in` files.
It should be a quadrouple $(S, T, N, P)$ where $S$ is the starting nonterminal, $T$ is the set of terminals, $N$ is the set of nonterminals and $P$ is the set of productions.
Even though they are mentioned as sets they are not utilized as sets in this program.
Their order matters in this program since the indexes are used to indentify the productions and symbols.

An example of something to parse is setting $q = 1$ and $k = 1$ and using `[0, 0, 1, 1, 1, 2, 2, 3]` (this corresponds to aabbbcc$) as inpput to `parse`.
It should return `[0, 2, 2, 1, 4, 4, 4, 3]` as in the [sequential-parser](../sequential-parser).

Next step:
 - Make a CST.
 - Start writing paper
 - Make a small lexer.

Sources:
[1] Ladislav Vagner and Bořivoj Melichar. 2007. Parallel LL parsing. Acta informatica 44, 1 (2007), 1–21.