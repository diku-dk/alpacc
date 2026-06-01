# Derivation Generation Optimization

## Problem

The original approach used in `derivableNLengths` was highly inefficient for generating parseable test inputs:

1. **BFS enumeration**: The function used breadth-first search to enumerate ALL possible derivations up to length n
2. **Exponential complexity**: For grammars with recursive productions, the number of derivations grows exponentially
3. **Performance issue**: Sequences longer than ~5 tokens would cause timeouts or very slow generation

## Solution

A new DFS-based approach that intelligently generates single derivations instead of enumerating all possibilities:

### Key Components

#### 1. Minimum Terminal Count Analysis (`minTerminalCounts`)

```haskell
minTerminalCounts :: (Ord nt, Ord t) => Grammar nt t -> Map nt Int
```

- Computes the minimum number of terminals each nonterminal can derive
- Uses fixed-point iteration to handle recursive grammars
- Helps identify productions that lead to termination

#### 2. Recursive Derivation Detection (`canDeriveRecursively`)

```haskell
canDeriveRecursively :: (Ord nt, Ord t) => Grammar nt t -> Set nt
```

- Identifies which nonterminals can appear in their own derivations
- Detects both direct and indirect recursion
- Used to find productions that can generate long derivations

#### 3. Smart Random Derivation (`generateRandomDerivation`)

```haskell
generateRandomDerivation ::
  (RandomGen g, Ord nt, Ord t, Show nt, Show t) =>
  g -> Int -> Grammar nt t -> (g, [t])
```

The core optimization that generates a single derivation intelligently:

**Strategy:**
1. **Expansion Phase**: When far from target length, prefer productions containing recursive nonterminals
2. **Convergence Phase**: When near target length, prefer productions with minimum terminal counts
3. **Dynamic Switching**: Automatically switches between strategies based on remaining length

**Benefits:**
- O(n) complexity instead of exponential
- Can generate arbitrarily long parseable inputs
- Maintains validity (all generated strings are parseable)

### Integration

The optimization is integrated into test generation with backward compatibility:

#### Parser Tests (`src/Alpacc/Test/Parser.hs`)

- New function: `generateParseableTokenSequenceFast`
- Automatically used when `n > 5` to avoid slow BFS
- Falls back to original method for short sequences

#### Lexer-Parser Combined Tests (`src/Alpacc/Test/LexerParser.hs`)

- New function: `generateParseableLexerParserInputFast`
- Generates token sequence first, then bytes that lex to those tokens
- Also uses threshold of `n > 5` for automatic selection

## Performance Improvement

| Sequence Length | Old Approach | New Approach |
|----------------|--------------|--------------|
| n ≤ 3          | Fast         | Fast         |
| n = 5          | Slow/Timeout | Fast         |
| n = 10         | Timeout      | Fast         |
| n = 50         | Impossible   | Fast         |
| n = 100        | Impossible   | Fast         |

## Backward Compatibility

- Original `derivableNLengths` function is preserved
- Original generation functions still available
- New fast functions only used when beneficial (n > 5)
- All existing tests continue to work unchanged

## Future Improvements

Potential enhancements:

1. **Adaptive threshold**: Dynamically determine when to switch based on grammar complexity
2. **Better convergence**: More sophisticated heuristics for choosing terminating productions
3. **Length control**: Ensure generated derivations match exact target length more reliably
4. **Memoization**: Cache grammar analysis results for repeated calls
