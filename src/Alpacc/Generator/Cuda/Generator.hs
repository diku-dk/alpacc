module Alpacc.Generator.Cuda.Generator
  ( generator,
  )
where

import Alpacc.Generator.Analyzer
import Alpacc.Generator.Cuda.Cudafy
import Alpacc.Generator.Cuda.Lexer qualified as Lexer
import Alpacc.Generator.Cuda.Parser qualified as Parser
import Alpacc.Types
import Data.FileEmbed
import Data.Text (Text)
import Data.Text qualified as Text hiding (Text)

common :: Text
common =
  Text.unlines
    [ $(embedStringFile "backends/cuda/common.cu"),
      $(embedStringFile "backends/cuda/scan.cu")
    ]

cudaCli :: Text
cudaCli = $(embedStringFile "backends/cuda/cli.cu")

generateTerminals :: UInt -> [Text] -> Text
generateTerminals terminal_type terminal_names =
  Text.unlines
    [ "#ifndef ALPACC_TERMINAL_T",
      "#define ALPACC_TERMINAL_T " <> cudafy terminal_type,
      "#endif",
      cudafyEnumRaw "terminal_t" "ALPACC_TERMINAL_T" terminal_names
    ]

-- | Emit an enum with a raw (already-computed) underlying-type token.
-- Used so terminal_t's underlying type can go through the ALPACC_TERMINAL_T
-- macro instead of being baked directly.
cudafyEnumRaw :: Text -> Text -> [Text] -> Text
cudafyEnumRaw name underlying names =
  "enum " <> name <> " : " <> underlying
    <> " {" <> Text.intercalate "," names <> "};"

-- | Emit index_t and length_t as #ifndef-guarded typedefs so a builder can
-- override either at nvcc invocation time via -DALPACC_INDEX_T=<type> or
-- -DALPACC_LENGTH_T=<type> without regenerating the source.  The codegen
-- defaults come from --index32 and the grammar's `length = <bits>` param.
indexTypeAlias :: Bool -> Maybe UInt -> Text
indexTypeAlias index32 mLenType =
  Text.unlines $
    (if index32 then ["#define INDEX32"] else [])
      ++ [ "#ifndef ALPACC_INDEX_T",
           "#ifdef INDEX32",
           "using index_t = int32_t;",
           "#else",
           "using index_t = int64_t;",
           "#endif",
           "#else",
           "using index_t = ALPACC_INDEX_T;",
           "#endif"
         ]
      ++ [ "#ifndef ALPACC_LENGTH_T",
           "using length_t = " <> lenT <> ";",
           "#else",
           "using length_t = ALPACC_LENGTH_T;",
           "#endif"
         ]
  where
    lenT = case mLenType of
      Nothing -> "std::make_unsigned_t<index_t>"
      Just U8  -> "uint8_t"
      Just U16 -> "uint16_t"
      Just U32 -> "uint32_t"
      Just U64 -> "uint64_t"

compileHint :: Text
compileHint = "// Compile: nvcc -O3 -std=c++17 -arch=native <this-file>.cu -o <output>"

-- | Emit device-dependent codegen constants as #ifndef-guarded #defines.
-- Both are baked at codegen time (from --shared-memory / --sm-arch, which
-- default to auto-probing the local device via nvcc).  The #ifndef guards
-- let a builder override at nvcc time with -DALPACC_SHARED_MEMORY=<n> and
-- -DALPACC_SM_ARCH=<n> without regenerating the source.  The kernel's
-- compile-time IPT/BS pick reads both.
deviceConsts :: Int -> Int -> Text
deviceConsts shmem sm =
  Text.unlines
    [ "#ifndef ALPACC_SHARED_MEMORY",
      "#define ALPACC_SHARED_MEMORY " <> Text.pack (show shmem),
      "#endif",
      "#ifndef ALPACC_SM_ARCH",
      "#define ALPACC_SM_ARCH "       <> Text.pack (show sm),
      "#endif"
    ]

auxiliary :: Bool -> Maybe UInt -> Int -> Int -> Analyzer [Text] -> Text
auxiliary index32 mLenType shmem sm analyzer =
  case analyzerKind analyzer of
    Lex lexer ->
      Text.unlines
        [ compileHint,
          Text.unlines (("// " <>) <$> meta analyzer),
          "#define HAS_LEXER",
          deviceConsts shmem sm,
          common,
          indexTypeAlias index32 mLenType,
          generateTerminals terminal_type terminal_names,
          Lexer.generateLexer lexer,
          cudaCli
        ]
    Parse parser ->
      Text.unlines
        [ compileHint,
          Text.unlines (("// " <>) <$> meta analyzer),
          "#define HAS_PARSER",
          deviceConsts shmem sm,
          common,
          indexTypeAlias index32 mLenType,
          generateTerminals terminal_type terminal_names,
          Parser.generateParser parser,
          cudaCli
        ]
    Both lexer parser ->
      Text.unlines
        [ compileHint,
          Text.unlines (("// " <>) <$> meta analyzer),
          "#define HAS_LEXER",
          "#define HAS_PARSER",
          "#define HAS_RAW_INPUT",
          deviceConsts shmem sm,
          common,
          indexTypeAlias index32 mLenType,
          generateTerminals terminal_type terminal_names,
          Lexer.generateLexer lexer,
          Parser.generateParser parser,
          cudaCli
        ]
  where
    terminal_type = terminalType analyzer
    terminal_names = terminalToName analyzer

generator :: Bool -> Maybe UInt -> Int -> Int -> Generator [Text]
generator index32 mLenType shmem sm =
  Generator
    { generate = auxiliary index32 mLenType shmem sm
    }
