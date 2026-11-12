// Sequential LLP reference parser (C backend).  Appended by the code
// generator after the #includes, the terminal_t/production_t/bracket_t
// typedefs and the grammar constants (Q, K, *_TERMINAL, HASH_TABLE_*,
// STACKS, PRODUCTIONS).  Mirrors pre_productions_int in futhark/parser.fut.
//
// Provides parse_test() and compute_parents() for use by run_case().

static bool is_left(bracket_t b) {
  return (b >> (8 * sizeof(bracket_t) - 1)) & 1;
}

static bracket_t unpack_bracket(bracket_t b) {
  return b & (bracket_t) ~((bracket_t) 1 << (8 * sizeof(bracket_t) - 1));
}

// FNV-1a over the q+k terminal window, as in parser.fut `hash`.
static uint64_t hash_key(const terminal_t *key) {
  uint64_t h = 14695981039346656037ULL;
  for (size_t i = 0; i < Q + K; i++)
    h = (h ^ (uint64_t) key[i]) * 1099511628211ULL;
  return h;
}

typedef struct {
  int64_t stack_start, stack_end, prod_start, prod_end;
} key_spans_t;

// Linear-probe lookup (parser.fut `lookup`); a key is only valid if it is
// found and no span component is -1 (`valid_keys`).
static bool lookup_key(const terminal_t *key, key_spans_t *spans) {
  uint64_t h = hash_key(key) % HASH_TABLE_SIZE;
  for (uint64_t i = 0; i < MAX_ITERS; i++) {
    if (HASH_TABLE_IS_VALID[h] &&
        memcmp(HASH_TABLE_KEYS[h], key, sizeof(terminal_t) * (Q + K)) == 0) {
      spans->stack_start = HASH_TABLE_STACKS_SPAN[h][0];
      spans->stack_end = HASH_TABLE_STACKS_SPAN[h][1];
      spans->prod_start = HASH_TABLE_PRODUCTIONS_SPAN[h][0];
      spans->prod_end = HASH_TABLE_PRODUCTIONS_SPAN[h][1];
      return spans->stack_start >= 0 && spans->stack_end >= 0 &&
             spans->prod_start >= 0 && spans->prod_end >= 0;
    }
    h = (h + 1) % HASH_TABLE_SIZE;
  }
  return false;
}

// One test case: n terminal ids -> production ids.  Returns validity; on
// success *prods_out (malloc'd, caller frees) holds *num_prods_out ids.
static bool parse_test(const terminal_t *tokens, uint64_t n,
                       production_t **prods_out, uint64_t *num_prods_out) {
  bool ok = false;
  uint64_t m = n + 2;
  terminal_t   *arr    = (terminal_t *)   malloc(m * sizeof(terminal_t));
  bracket_t    *stack  = (bracket_t *)    malloc(m * MAX_BRACKETS_PER_POSITION * sizeof(bracket_t));
  production_t *prods  = (production_t *) malloc(m * MAX_PRODS_PER_POSITION    * sizeof(production_t));
  terminal_t key[Q + K];
  uint64_t np = 0;
  int64_t top = -1;

  arr[0] = START_TERMINAL;
  for (uint64_t i = 0; i < n; i++)
    arr[i + 1] = tokens[i];
  arr[m - 1] = END_TERMINAL;

  for (uint64_t i = 0; i < m; i++) {
    for (uint64_t j = 0; j < Q + K; j++) {
      uint64_t idx = i + j;
      key[j] = (idx < Q || idx >= m + Q) ? EMPTY_TERMINAL : arr[idx - Q];
    }
    key_spans_t spans;
    if (!lookup_key(key, &spans))
      goto done;
    for (int64_t j = spans.stack_start; j < spans.stack_end; j++) {
      bracket_t b = STACKS[j];
      if (is_left(b)) {
        stack[++top] = b;
      } else {
        if (top < 0 || unpack_bracket(stack[top]) != unpack_bracket(b))
          goto done;
        top--;
      }
    }
    for (int64_t j = spans.prod_start; j < spans.prod_end; j++)
      prods[np++] = PRODUCTIONS[j];
  }
  if (top != -1)
    goto done;
  *prods_out = prods;
  *num_prods_out = np;
  prods = NULL;
  ok = true;

done:
  free(arr);
  free(stack);
  free(prods);
  return ok;
}

#ifdef ALPACC_WITH_TREE
// Compute the parent index of each production node.  Mirrors `parents` in
// futhark/parser.fut.  Used by the combined lexer+parser run_case().
static void compute_parents(const production_t *prods, uint64_t np,
                            index_t *parents_out) {
  uint64_t *remaining = (uint64_t *) malloc(np * sizeof(uint64_t));
  uint64_t *stk       = (uint64_t *) malloc(np * sizeof(uint64_t));
  int64_t top = -1;
  for (uint64_t i = 0; i < np; i++) {
    uint64_t arity = PRODUCTION_TO_ARITY[prods[i]];
    remaining[i] = arity;
    parents_out[i] = (top >= 0) ? stk[top] : 0;
    if (arity > 0) stk[++top] = i;
    while (top >= 0 && remaining[stk[top]] == 0) top--;
    if (top >= 0 && i > 0) remaining[stk[top]]--;
  }
  free(remaining);
  free(stk);
}

// Compact the production list to a productions-only tree: drop terminal
// slots, remap parents into the compacted numbering, and record for each
// lexeme the compacted index of its parent node.  Returns false if the
// number of terminal slots does not match num_lexemes.  Caller provides
// tree_prods/tree_parents of capacity np and token_parents of capacity
// num_lexemes; on success the tree arrays hold np - num_lexemes entries.
static bool compact_tree(const production_t *prods, uint64_t np,
                         const index_t *parents, uint64_t num_lexemes,
                         production_t *tree_prods, index_t *tree_parents,
                         index_t *token_parents) {
  uint64_t *t_incl = (uint64_t *) malloc(np * sizeof(uint64_t));
  uint64_t count = 0;
  for (uint64_t i = 0; i < np; i++) {
    if (PRODUCTION_TO_TERMINAL_IS_VALID[prods[i]]) count++;
    t_incl[i] = count;
  }
  if (count != num_lexemes) {
    free(t_incl);
    return false;
  }
  for (uint64_t i = 0; i < np; i++) {
    index_t p_old = parents[i];
    index_t p_new = p_old - (index_t) t_incl[p_old];
    if (PRODUCTION_TO_TERMINAL_IS_VALID[prods[i]]) {
      token_parents[t_incl[i] - 1] = p_new;
    } else {
      uint64_t j = i - t_incl[i];
      tree_prods[j] = prods[i];
      tree_parents[j] = p_new;
    }
  }
  free(t_incl);
  return true;
}
#endif /* ALPACC_WITH_TREE */
