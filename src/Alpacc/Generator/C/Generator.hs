module Alpacc.Generator.C.Generator
  ( generator,
  )
where

import Alpacc.Generator.Analyzer
import Alpacc.Generator.C.Lexer qualified as Lexer
import Alpacc.Generator.C.Parser qualified as Parser
import Alpacc.Generator.Cuda.Cudafy
import Alpacc.Types
import Data.FileEmbed
import Data.Text (Text)
import Data.Text qualified as Text

cCli :: Text
cCli = $(embedStringFile "backends/c/cli.c")

cParser :: Text
cParser = $(embedStringFile "backends/c/parser.c")

cLexer :: Text
cLexer = $(embedStringFile "backends/c/lexer.c")

includes :: Text
includes =
  Text.unlines
    [ "#define _POSIX_C_SOURCE 200809L",
      "#include <stdint.h>",
      "#include <stdbool.h>",
      "#include <stdio.h>",
      "#include <stdlib.h>",
      "#include <string.h>",
      "#include <time.h>"
    ]

indexTypedef :: Bool -> Maybe UInt -> Text
indexTypedef index32 mLenType =
  Text.unlines $
    (if index32 then ["#define INDEX32"] else [])
      ++ [ "#ifdef INDEX32",
           "typedef int32_t index_t;",
           "#else",
           "typedef int64_t index_t;",
           "#endif",
           "typedef " <> lenT <> " length_t;"
         ]
  where
    lenT = case mLenType of
      Nothing  -> if index32 then "uint32_t" else "uint64_t"
      Just U8  -> "uint8_t"
      Just U16 -> "uint16_t"
      Just U32 -> "uint32_t"
      Just U64 -> "uint64_t"

-- Forward declarations for the I/O helpers defined in cli.c.
-- These let the generated test-case code call them without needing
-- cli.c to come first.
ioForwardDecls :: Text
ioForwardDecls = Text.unlines
  [ "static void     write_u64le(FILE *f, uint64_t v);"
  ]

-- Parser-only: payload is n native terminal_t token ids (host byte order).
parserTestCase :: Text
parserTestCase = Text.unlines
  [ "#define INPUT_BYTES(n) ((n) * sizeof(terminal_t))"
  , "static void run_case(uint64_t n, const uint8_t *buf, FILE *out) {"
  , "  terminal_t *tokens = (terminal_t *) malloc(n * sizeof(terminal_t));"
  , "  memcpy(tokens, buf, n * sizeof(terminal_t));"
  , "  production_t *prods; uint64_t num_prods;"
  , "  if (parse_test(tokens, n, &prods, &num_prods)) {"
  , "    fputc(1, out);"
  , "    write_u64le(out, num_prods);"
  , "    fwrite(prods, sizeof(production_t), num_prods, out);"
  , "    free(prods);"
  , "  } else { fputc(0, out); }"
  , "  free(tokens);"
  , "}"
  , "static void print_layout(FILE *f) {"
  , "  fprintf(f, \"terminal_t=%zu\\n\", sizeof(terminal_t));"
  , "  fprintf(f, \"production_t=%zu\\n\", sizeof(production_t));"
  , "  fprintf(f, \"index_t=%zu\\n\", sizeof(index_t));"
  , "}"
  ]

-- Lexer-only: payload is n raw bytes.
lexerTestCase :: Text
lexerTestCase = Text.unlines
  [ "#define INPUT_BYTES(n) (n)"
  , "static void run_case(uint64_t n, const uint8_t *buf, FILE *out) {"
  , "  lexeme_t *lexemes; uint64_t num_lexemes;"
  , "  if (lex_string(buf, n, &lexemes, &num_lexemes)) {"
  , "    fputc(1, out);"
  , "    write_u64le(out, num_lexemes);"
  , "    for (uint64_t i = 0; i < num_lexemes; i++) {"
  , "      fwrite(&lexemes[i].terminal, sizeof(terminal_t), 1, out);"
  , "      fwrite(&lexemes[i].start, sizeof(index_t), 1, out);"
  , "      fwrite(&lexemes[i].length, sizeof(length_t), 1, out);"
  , "    }"
  , "    free(lexemes);"
  , "  } else { fputc(0, out); }"
  , "}"
  , "static void print_layout(FILE *f) {"
  , "  fprintf(f, \"terminal_t=%zu\\n\", sizeof(terminal_t));"
  , "  fprintf(f, \"index_t=%zu\\n\", sizeof(index_t));"
  , "  fprintf(f, \"length_t=%zu\\n\", sizeof(length_t));"
  , "}"
  ]

-- Combined: payload is n raw bytes; lex then parse, then emit the SoA
-- record: tokens (ids, starts, ends) followed by the compacted
-- productions-only tree (prods, parents) and per-token parent indices.
-- compute_parents()/compact_tree() are defined in c/parser.c.
bothTestCase :: Text
bothTestCase = Text.unlines
  [ "#define INPUT_BYTES(n) (n)"
  , "static void run_case(uint64_t n, const uint8_t *buf, FILE *out) {"
  , "  lexeme_t *lexemes; uint64_t num_lexemes;"
  , "  if (!lex_string(buf, n, &lexemes, &num_lexemes)) { fputc(0, out); return; }"
  , "  terminal_t *tokens = (terminal_t *) malloc(num_lexemes * sizeof(terminal_t));"
  , "  for (uint64_t i = 0; i < num_lexemes; i++) tokens[i] = lexemes[i].terminal;"
  , "  production_t *prods; uint64_t num_prods;"
  , "  if (!parse_test(tokens, num_lexemes, &prods, &num_prods)) {"
  , "    free(tokens); free(lexemes); fputc(0, out); return;"
  , "  }"
  , "  free(tokens);"
  , "  index_t *parents = (index_t *) malloc(num_prods * sizeof(index_t));"
  , "  compute_parents(prods, num_prods, parents);"
  , "  production_t *tree_prods    = (production_t *) malloc(num_prods * sizeof(production_t));"
  , "  index_t      *tree_parents  = (index_t *)      malloc(num_prods * sizeof(index_t));"
  , "  index_t      *token_parents = (index_t *)      malloc(num_lexemes * sizeof(index_t));"
  , "  if (!compact_tree(prods, num_prods, parents, num_lexemes,"
  , "                    tree_prods, tree_parents, token_parents)) {"
  , "    fputc(0, out);"
  , "  } else {"
  , "    uint64_t num_nodes = num_prods - num_lexemes;"
  , "    fputc(1, out);"
  , "    write_u64le(out, num_lexemes);"
  , "    for (uint64_t i = 0; i < num_lexemes; i++)"
  , "      fwrite(&lexemes[i].terminal, sizeof(terminal_t), 1, out);"
  , "    for (uint64_t i = 0; i < num_lexemes; i++)"
  , "      fwrite(&lexemes[i].start, sizeof(index_t), 1, out);"
  , "    for (uint64_t i = 0; i < num_lexemes; i++)"
  , "      fwrite(&lexemes[i].length, sizeof(length_t), 1, out);"
  , "    write_u64le(out, num_nodes);"
  , "    fwrite(tree_prods, sizeof(production_t), num_nodes, out);"
  , "    fwrite(tree_parents, sizeof(index_t), num_nodes, out);"
  , "    fwrite(token_parents, sizeof(index_t), num_lexemes, out);"
  , "  }"
  , "  free(prods); free(parents); free(lexemes);"
  , "  free(tree_prods); free(tree_parents); free(token_parents);"
  , "}"
  , "static void print_layout(FILE *f) {"
  , "  fprintf(f, \"terminal_t=%zu\\n\", sizeof(terminal_t));"
  , "  fprintf(f, \"production_t=%zu\\n\", sizeof(production_t));"
  , "  fprintf(f, \"index_t=%zu\\n\", sizeof(index_t));"
  , "  fprintf(f, \"length_t=%zu\\n\", sizeof(length_t));"
  , "}"
  ]

auxiliary :: Bool -> Maybe UInt -> Analyzer [Text] -> Text
auxiliary index32 mLenType analyzer =
  case analyzerKind analyzer of
    Lex lexer ->
      Text.unlines
        [ Text.unlines (("// " <>) <$> meta analyzer),
          includes,
          "typedef " <> cudafy (terminalType analyzer) <> " terminal_t;",
          indexTypedef index32 mLenType,
          ioForwardDecls,
          Lexer.generateLexer lexer,
          cLexer,
          lexerTestCase,
          cCli
        ]
    Parse parser ->
      Text.unlines
        [ Text.unlines (("// " <>) <$> meta analyzer),
          includes,
          "typedef " <> cudafy (terminalType analyzer) <> " terminal_t;",
          indexTypedef index32 mLenType,
          ioForwardDecls,
          Parser.generateParser parser,
          cParser,
          parserTestCase,
          cCli
        ]
    Both lexer parser ->
      Text.unlines
        [ Text.unlines (("// " <>) <$> meta analyzer),
          includes,
          "typedef " <> cudafy (terminalType analyzer) <> " terminal_t;",
          indexTypedef index32 mLenType,
          ioForwardDecls,
          Lexer.generateLexer lexer,
          cLexer,
          Parser.generateParserWithTree parser,
          cParser,
          bothTestCase,
          cCli
        ]

generator :: Bool -> Maybe UInt -> Generator [Text]
generator index32 mLenType =
  Generator
    { generate = auxiliary index32 mLenType
    }
