# Table-driven LL(1) parser
A primitive table-driven LL(1) parser [1, 68] which prints out the productions used for parsing a
given string. It is primitive since it assumes that you have defined a context-free grammar which is
well-defined. The least the parser do is throw an error if the input could not be parsed. This
parser was created as an exercise to help the author understand table-driven LL(1) parsing.

Sources:

- [1] Mogensen, T. Æ. (2017). Introduction to Compiler Design. Undergraduate Topics in Computer Science. https://doi.org/10.1007/978-3-319-66966-3

## Defining your own grammar

An example of a grammar defined in a json file is.
```
{
    "start": "T",
    "extendStart": "T'",
    "extendEnd": "$",
    "nonterminals": ["T", "R"],
    "terminals": ["a", "b", "c"],
    "grammar": {
        "T": [["R"], ["a", "T", "c"]],
        "R": [[], ["b", "R"]]
    }
}
```

* `grammar`
  The `grammar` field would define a grammar like this.
  $$
  T \to aTc \\
  T \to R \\
  R \to  \\
  R \to bR \\ 
  $$
  The list of list is used much like the notation $A -> c \:| \:d$.
* `start` the first production to be used.
* `terminals` and `nonterminals` are the once used in `grammar`.
* `extendStart` and `extendEnd` should just be distinct from the `terminals` and `nonterminals` they
  are used to extend the grammar such that FOLLOW can be used.

## Usage
Start by defining a grammar in a json file and do.
```
$ cabal run sequential-parser grammar.json
```
The program will then ask for a string to be parsed. An example is the grammar in `grammar.json` a
valid input could be `aabbbcc$`.