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
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text hiding (Text)

common :: Text
common = $(embedStringFile "cuda/common.cu")

generateTerminals :: UInt -> [Text] -> Text
generateTerminals terminal_type terminal_names =
  Text.unlines
    [ cudafyEnum "terminal_t" terminal_type terminal_names
    ]

auxiliary :: Analyzer [Text] -> Text
auxiliary analyzer =
  case analyzerKind analyzer of
    Lex lexer ->
      Text.unlines
        [ Text.unlines (("// " <>) <$> meta analyzer),
          common,
          generateTerminals terminal_type terminal_names,
          Lexer.generateLexer lexer,
          Text.pack
            [i|
int main(int32_t argc, char *argv[]) {
  constexpr uint32_t SHARED_MEMORY = 49152;
  
  #ifdef DEBUG
    constexpr uint32_t BLOCK_SIZE = 32;
    constexpr uint32_t CHUNK_SIZE = 64;
    constexpr uint32_t ITEMS_PER_THREAD = 2;
  #else
    constexpr uint32_t BLOCK_SIZE = 256;
    constexpr uint32_t CHUNK_SIZE = 100 * (1 << 20); // 100 MiB
    constexpr uint32_t ITEMS_PER_THREAD = 
        calculate_lexer_max_items_per_thread<uint32_t, state_t, BLOCK_SIZE, SHARED_MEMORY>();
  #endif
  
  return lexer_stream<WriteAscii, CHUNK_SIZE, BLOCK_SIZE, ITEMS_PER_THREAD>(WriteAscii());
}|]
        ]
    Parse parser ->
      Text.unlines
        [ Text.unlines (("// " <>) <$> meta analyzer),
          common,
          generateTerminals terminal_type terminal_names,
          Parser.generateParser parser,
          Text.pack
            [i|
|]
        ]
    Both lexer parser ->
      Text.unlines
        [ Text.unlines (("// " <>) <$> meta analyzer),
          common,
          generateTerminals terminal_type terminal_names,
          Lexer.generateLexer lexer,
          Parser.generateParser parser,
          Text.pack
            [i|
|]
        ]
  where
    terminal_type = terminalType analyzer
    terminal_names = terminalToName analyzer

generator :: Generator [Text]
generator =
  Generator
    { generate = auxiliary
    }
