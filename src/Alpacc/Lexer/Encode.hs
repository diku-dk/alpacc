module Alpacc.Lexer.Encode
  ( intParallelLexer,
    IntParallelLexer (..),
    ParallelLexerMasks (..),
    extEndoType,
  )
where

import Alpacc.Lexer.ParallelLexing
import Alpacc.Types
import Control.Monad
import Data.Bits
import Data.Either.Extra
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty hiding (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map hiding (Map)
import Data.Text (Text)

errorMessage :: Text
errorMessage = "Error: Happend during Parallel Lexing genration, contact a maintainer."

data Mask64 = Mask64
  { mask :: !Int,
    offset :: !Int
  }
  deriving (Show, Eq, Ord)

newtype Masks64 = Masks64 (NonEmpty Mask64)

findBitSize :: Int -> Int
findBitSize = (int_size -) . countLeadingZeros . max 1 . pred
  where
    int_size = finiteBitSize (zeroBits :: Int)

masks :: [Int] -> Either Text Masks64
masks sizes = do
  unless (all (0 <) sizes) $ Left "Error: Negative sizes were used to encode the masks for the states in a data parallel lexer. This should not happen, contact a maintainer."
  unless (sum bit_sizes <= 64) $ Left "Error: There are too many tokens and/or states to create a data parallel lexer."
  let offsets = init $ List.scanl' (+) 0 bit_sizes -- Exclusive scan.
  let unshifted_masks = pred . shift 1 <$> bit_sizes
  let _masks = zipWith shift unshifted_masks offsets
  pure $ Masks64 $ NonEmpty.fromList $ zipWith Mask64 _masks offsets
  where
    bit_sizes = findBitSize <$> sizes

data ParallelLexerMasks = ParallelLexerMasks
  { tokenMask :: !Int,
    tokenOffset :: !Int,
    indexMask :: !Int,
    indexOffset :: !Int,
    producingMask :: !Int,
    producingOffset :: !Int
  }
  deriving (Eq, Ord, Show)

extEndoType :: ParallelLexer t k -> Either Text UInt
extEndoType (ParallelLexer {endomorphismsSize = e, tokenSize = t}) =
  maybeToEither errorMessage . toIntType . (2 ^) . sum $ findBitSize <$> [e, t, 1]

parallelLexerToMasks64 :: ParallelLexer t k -> Either Text Masks64
parallelLexerToMasks64 (ParallelLexer {endomorphismsSize = e, tokenSize = t}) =
  masks [e, t, 1]

unsafeEncodeMasks64 :: (Bits i, Integral i) => Masks64 -> NonEmpty i -> i
unsafeEncodeMasks64 (Masks64 ms) elems =
  let x = offset <$> ms
   in sum $ NonEmpty.zipWith shift elems x

masks64ToParallelLexerMasks :: Masks64 -> Either Text ParallelLexerMasks
masks64ToParallelLexerMasks (Masks64 ls) = do
  let ls0 = NonEmpty.toList ls
  (idx, ls1) <- aux ls0
  (token, ls2) <- aux ls1
  (produce, _ls3) <- aux ls2
  pure $
    ParallelLexerMasks
      { tokenMask = mask token,
        tokenOffset = offset token,
        indexMask = mask idx,
        indexOffset = offset idx,
        producingMask = mask produce,
        producingOffset = offset produce
      }
  where
    aux = maybeToEither errorMessage . List.uncons

parallelLexerMasksToMasks64 :: ParallelLexerMasks -> Masks64
parallelLexerMasksToMasks64 lexer_masks =
  Masks64 $ NonEmpty.fromList elems
  where
    ParallelLexerMasks
      { indexMask = mask_index,
        indexOffset = offset_index,
        tokenMask = mask_token,
        tokenOffset = offset_token,
        producingMask = mask_produce,
        producingOffset = offset_produce
      } = lexer_masks
    elems =
      [ Mask64 {mask = mask_index, offset = offset_index},
        Mask64 {mask = mask_token, offset = offset_token},
        Mask64 {mask = mask_produce, offset = offset_produce}
      ]

lexerMasks :: ParallelLexer t k -> Either Text ParallelLexerMasks
lexerMasks lexer = do
  ms <- parallelLexerToMasks64 lexer
  masks64ToParallelLexerMasks ms

encodeEndoData ::
  (Ord k, Bits i, Integral i) =>
  ParallelLexerMasks ->
  Map (Maybe k) i ->
  EndoData k ->
  i
encodeEndoData lexer_masks to_int endo_data =
  do
    unsafeEncodeMasks64 ms
    $ NonEmpty.fromList [fromIntegral e, t, p]
  where
    ms = parallelLexerMasksToMasks64 lexer_masks
    EndoData
      { endo = e,
        token = maybe_token,
        isProducing = produce
      } = endo_data
    p = fromIntegral $ fromEnum produce
    t = to_int Map.! maybe_token

data IntParallelLexer t i = IntParallelLexer
  { parLexer :: !(ParallelLexer t i),
    parMasks :: !ParallelLexerMasks
  }
  deriving (Show, Eq, Ord)

intParallelLexer ::
  (Ord t, Ord k, Bits i, Integral i) =>
  Map (Maybe k) i ->
  ParallelLexer t (EndoData k) ->
  Either Text (IntParallelLexer t i)
intParallelLexer to_int parallel_lexer = do
  ms <- lexerMasks parallel_lexer
  let encode = encodeEndoData ms to_int
  let new_compositions = fmap encode <$> compositions parallel_lexer
  let new_endomorphims = encode <$> endomorphisms parallel_lexer
  let new_identity = encode $ identity parallel_lexer
  let new_dead = encode $ dead parallel_lexer
  let new_parallel_lexer =
        parallel_lexer
          { compositions = new_compositions,
            endomorphisms = new_endomorphims,
            identity = new_identity,
            dead = new_dead
          }
  pure $
    IntParallelLexer
      { parLexer = new_parallel_lexer,
        parMasks = ms
      }
