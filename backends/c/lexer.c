// Sequential single-pass lexer (C backend).  Appended by the code generator
// after the lexer constants (TO_STATE, COMPOSE, ACCEPT, masks, IDENTITY,
// DEAD_TOKEN, optionally IGNORE_TOKEN).
//
// Provides lex_string() for use by the generated main() in all modes.
//
// Encoding: each state_t packs three fields via bit masks:
//   index   = (state & ENDO_MASK)     >> ENDO_OFFSET
//   terminal= (state & TERMINAL_MASK) >> TERMINAL_OFFSET
//   produce = (state & PRODUCE_MASK)  >> PRODUCE_OFFSET

static state_t compose(state_t a, state_t b) {
  uint64_t ai = (uint64_t)((a & ENDO_MASK) >> ENDO_OFFSET);
  uint64_t bi = (uint64_t)((b & ENDO_MASK) >> ENDO_OFFSET);
  return COMPOSE[ai * NUM_STATES + bi];
}

static terminal_t state_terminal(state_t s) {
  return (terminal_t)((s & TERMINAL_MASK) >> TERMINAL_OFFSET);
}

static bool state_produce(state_t s) {
  return (bool)((s & PRODUCE_MASK) >> PRODUCE_OFFSET);
}

static bool state_accept(state_t s) {
  return ACCEPT[(s & ENDO_MASK) >> ENDO_OFFSET];
}

typedef struct {
  terminal_t terminal;
  index_t    start;
  length_t   length;
} lexeme_t;

// Lex `n` bytes from `str`.  On success returns a malloc'd array of lexemes
// in *out, sets *num_out, returns true.  Returns false if lexing fails or a
// token length overflows length_t.
static bool lex_string(const uint8_t *str, uint64_t n,
                       lexeme_t **out, uint64_t *num_out) {
  if (n == 0) {
    *out = NULL;
    *num_out = 0;
    return true;
  }

  // Single left-to-right pass: maintain current composed state and the start
  // of the current token.
  uint64_t cap = 16;
  lexeme_t *result = (lexeme_t *) malloc(cap * sizeof(lexeme_t));
  uint64_t count = 0;
  state_t cur = TO_STATE[(uint8_t) str[0]];
  // Compose with identity for position 0 (matches traverse in lexer.fut).
  cur = compose(IDENTITY, cur);
  uint64_t tok_start = 0;

  for (uint64_t i = 1; i <= n; i++) {
    state_t next = (i < n) ? TO_STATE[(uint8_t) str[i]] : IDENTITY;
    bool is_boundary = (i == n) || state_produce(compose(cur, next));

    if (is_boundary) {
      terminal_t t = state_terminal(cur);
#ifdef IGNORE_TOKEN
      if ((uint64_t) t != (uint64_t) IGNORE_TOKEN) {
#else
      if (true) {
#endif
        uint64_t tok_len = i - tok_start;
        if (tok_len > (uint64_t)(length_t)(-1)) {
          free(result);
          return false;
        }
        if (count == cap) {
          cap *= 2;
          result = (lexeme_t *) realloc(result, cap * sizeof(lexeme_t));
        }
        result[count].terminal = t;
        result[count].start    = tok_start;
        result[count].length   = (length_t) tok_len;
        count++;
      }
      tok_start = i;
    }

    if (i < n)
      cur = compose(cur, next);
  }

  if (!state_accept(cur)) {
    free(result);
    return false;
  }

  *out = result;
  *num_out = count;
  return true;
}
