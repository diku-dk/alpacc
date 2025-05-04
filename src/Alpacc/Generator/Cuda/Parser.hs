module Alpacc.Generator.Cuda.Parser
  ( generateParser,
  )
where

import Alpacc.Generator.Cuda.Cudafy
import Alpacc.Generator.Util
import Alpacc.Grammar
import Alpacc.HashTable
import Alpacc.LLP (Bracket (..))
import Alpacc.Types
import Control.DeepSeq
import Data.Array.Base as ABase
import Data.Bifunctor qualified as BI
import Data.Composition
import Data.Either.Extra (maybeToEither)
import Data.FileEmbed
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple.Extra

cudaParser :: Text
cudaParser = $(embedStringFile "cuda/parser.cu")

generateParser ::
  (NFData t, NFData nt, Ord nt, Show nt, Show t, Ord t) =>
  Int ->
  Int ->
  Grammar (Either nt t) t ->
  Map (Symbol (AugmentedNonterminal (Either nt t)) (AugmentedTerminal t)) Integer ->
  Either Text (Text, IInt)
generateParser q k grammar symbol_index_map = do
  (start_terminal, end_terminal) <- startEndIndex symbol_index_map
  -- table <- llpParserTableWithStartsHomomorphisms q k grammar
  -- bracket_type <- findBracketIntegral symbol_index_map
  -- production_type <- findProductionIntegral $ productions grammar
  return . (,I64) $
    cudaParser
      <> Text.strip
        ( Text.pack
            [i|
const size_t Q = #{q};
const size_t K = #{k};
|]
        )
