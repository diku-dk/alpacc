module Alpacc.Lexer.Encode
  ( intParallelLexer
  , IntParallelLexer (..)
  , ParallelLexerMasks (..)
  , extEndoType
  )
where

import Alpacc.Types
import Alpacc.Lexer.ParallelLexing
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map hiding (Map)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty hiding (NonEmpty)
import Data.Either.Extra
import Data.Bits
import Control.Monad

errorMessage :: String
errorMessage = "Error: Happend during Parallel Lexing genration, contact a maintainer."

data Mask64 =
  Mask64
  { mask :: !Int
  , offset :: !Int
  } deriving (Show, Eq, Ord)

newtype Masks64 = Masks64 (NonEmpty Mask64)

findBitSize :: Int -> Int
findBitSize = (int_size-) . countLeadingZeros . max 1 . pred
  where
    int_size = finiteBitSize (zeroBits :: Int)

masks :: [Int] -> Either String Masks64
masks sizes = do
  unless (all (0<) sizes) $ Left "Error: Negative sizes were used to encode the masks for the states in a data parallel lexer. This should not happen, contact a maintainer."
  unless (sum bit_sizes <= 64) $ Left "Error: There are too many tokens and/or states to create a data parallel lexer."
  let offsets = init $ List.scanl' (+) 0 bit_sizes -- Exclusive scan.
  let _masks = zipWith shift offsets bit_sizes
  pure $ Masks64 $ NonEmpty.fromList $ zipWith Mask64 _masks offsets
  where
    bit_sizes = findBitSize <$> sizes

data ParallelLexerMasks =
  ParallelLexerMasks
  { tokenMask :: !Int
  , tokenOffset :: !Int
  , indexMask :: !Int
  , indexOffset :: !Int
  , producingMask :: !Int
  , producingOffset :: !Int
  } deriving (Eq, Ord, Show)

extEndoType :: ParallelLexer t k -> Either String UInt
extEndoType (ParallelLexer { endomorphismsSize = a, tokenSize = b }) =
  maybeToEither errorMessage . toIntType . (2^)  . sum $ findBitSize <$> [a, b, 1]

parallelLexerToMasks64 :: ParallelLexer t k -> Either String Masks64
parallelLexerToMasks64 (ParallelLexer { endomorphismsSize = e, tokenSize = t }) =
  masks [e, t, 1]

encodeMasks64 :: Masks64 -> NonEmpty Int -> Either String Int
encodeMasks64 (Masks64 ms) elems
  | NonEmpty.length ms == NonEmpty.length elems =
    let x = offset <$> ms
    in pure $ sum $ NonEmpty.zipWith (flip shift) elems x 
  | otherwise = Left errorMessage

masks64ToParallelLexerMasks :: Masks64 -> Either String ParallelLexerMasks 
masks64ToParallelLexerMasks (Masks64 ls) = do
  let ls0 = NonEmpty.toList ls
  (idx, ls1) <- aux ls0
  (token, ls2) <- aux ls1
  (produce, _ls3) <- aux ls2
  pure $
    ParallelLexerMasks
    { tokenMask = mask token
    , tokenOffset = offset token
    , indexMask = mask idx
    , indexOffset = offset idx
    , producingMask = mask produce
    , producingOffset = offset produce
    }
  where
    aux = maybeToEither errorMessage . List.uncons

parallelLexerMasksToMasks64 :: ParallelLexerMasks -> Masks64
parallelLexerMasksToMasks64 lexer_masks =
  Masks64 $ NonEmpty.fromList elems
  where
    ParallelLexerMasks
      { indexMask = mask_index
      , indexOffset = offset_index
      , tokenMask = mask_token
      , tokenOffset = offset_token
      , producingMask = mask_produce
      , producingOffset = offset_produce
      } = lexer_masks
    elems =
      [Mask64 {mask = mask_index, offset = offset_index}
      ,Mask64 {mask = mask_token, offset = offset_token}
      ,Mask64 {mask = mask_produce, offset = offset_produce}]

lexerMasks :: ParallelLexer t k -> Either String ParallelLexerMasks
lexerMasks lexer = do
  ms <- parallelLexerToMasks64 lexer
  masks64ToParallelLexerMasks ms

encodeEndoData ::
  Ord k =>
  ParallelLexerMasks ->
  Map (Maybe k) Int ->
  ExtEndoData k ->
  Either String Int
encodeEndoData lexer_masks to_int endo_data = do
  t <- findInt maybe_token 
  encodeMasks64 ms $ NonEmpty.fromList [e, t, p]
  where
    ms = parallelLexerMasksToMasks64 lexer_masks
    ExtEndoData
      { endo = e
      , token = maybe_token
      , isProducing = produce
      } = endo_data
    p = fromEnum produce
    findInt = maybeToEither errorMessage . flip Map.lookup to_int

data IntParallelLexer t =
  IntParallelLexer
  { parLexer :: !(ParallelLexer t Int)
  , parMasks :: !ParallelLexerMasks
  } deriving (Show, Eq, Ord)

intParallelLexer ::
  Ord k =>
  Map (Maybe k) Int ->
  ParallelLexer t (ExtEndoData k) ->
  Either String (IntParallelLexer t)
intParallelLexer to_int parallel_lexer = do
  ms <- lexerMasks parallel_lexer
  let encode = encodeEndoData ms to_int
  new_compositions <- mapM encode $ compositions parallel_lexer
  new_endomorphims <- mapM encode $ endomorphisms parallel_lexer
  new_identity <- encode $ identity parallel_lexer
  new_dead <- encode $ dead parallel_lexer
  let new_parallel_lexer =
        parallel_lexer
        { compositions = new_compositions
        , endomorphisms = new_endomorphims
        , identity = new_identity
        , dead = new_dead
        }
  pure $
    IntParallelLexer
    { parLexer = new_parallel_lexer
    , parMasks = ms
    }
