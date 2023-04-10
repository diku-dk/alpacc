# ParallelParser
This is an early stage LLP(q, k) table generator [1] which computes a list of indexes productions rules.
An example use of the grammr is.
```
cabal run parallel-parser -- grammars/paper_grammar.cg -q 1 -k 1
```
This will create a source file with a parser written in [Futhark](https://futhark-lang.org/) if the grammr is not LLP(q,k) then the parser cannot be created.
The `parse` function in the parser takes an array of indexes that corresponds to the indexes of the terminals in the `.cg` file where 0 is the starting index.
`parse` then returns a list of indexes that corresponds to the indexes of the productions.

To see examples of how to define a context-free grammar look in the `.in` files.
It should be a quadrouple $(S, T, N, P)$ where $S$ is the starting nonterminal, $T$ is the set of terminals, $N$ is the set of nonterminals and $P$ is the set of productions.
Even though they are mentioned as sets they are not utilized as sets in this program.
Their order matters in this program since the indexes are used to indentify the productions and symbols.

Certain charactors needs to be escaped in the `.cg` file the following substrings maps to the strings on the right.
- "\\\\" $\mapsto$ "\\"
- "\\," $\mapsto$ ","
- "\\}" $\mapsto$ "}"
- "\\t" $\mapsto$ t"
- "\\r" $\mapsto$ r"
- "\\n" $\mapsto$ n"
- "\\s" $\mapsto$ " "
- "\\" $\mapsto$ ""

An example of something to parse is setting $q = 1$ and $k = 1$ and using `[0, 0, 1, 1, 1, 2, 2, 3]` (this corresponds to aabbbcc$) as input to `parse`.
It should return `[0, 2, 2, 1, 4, 4, 4, 3]` as in the [sequential-parser](https://github.com/WilliamDue/sequential-parser).

Next step:
 - Make a CST.
 - Start writing paper
 - Make a small lexer.
 - remove left recursion

Sources:
[1] Ladislav Vagner and Bořivoj Melichar. 2007. Parallel LL parsing. Acta informatica 44, 1 (2007), 1–21.