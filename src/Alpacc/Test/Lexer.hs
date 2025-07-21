module Alpacc.Test.Lexer
  ( testGenRegEx,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.DFAParallelLexer
import Alpacc.Lexer.Encode
import Alpacc.Lexer.ParallelLexing
import Alpacc.Lexer.RegularExpression
import Alpacc.Types
import Control.Monad
import Data.Either.Extra
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Gen,
    elements,
    generate,
    listOf,
    oneof,
    sized,
  )

mkLexer :: CFG -> Either Text (DFALexer Word8 Integer T)
mkLexer cfg = do
  spec <- dfaCharToWord8 <$> cfgToDFALexerSpec cfg
  pure $ lexerDFA (0 :: Integer) spec

genRegEx :: (Arbitrary c) => Int -> Gen (RegEx c)
genRegEx size
  | size <= 1 =
      oneof
        [ pure Epsilon,
          Literal <$> arbitrary,
          Range <$> arbitrary
        ]
  | otherwise =
      oneof
        [ pure Epsilon,
          Literal <$> arbitrary,
          Range <$> arbitrary,
          Star <$> genRegEx (size `div` 2),
          Concat <$> genRegEx (size `div` 2) <*> genRegEx (size `div` 2),
          Alter <$> genRegEx (size `div` 2) <*> genRegEx (size `div` 2)
        ]

instance (Arbitrary c) => Arbitrary (RegEx c) where
  arbitrary = sized auxiliary
    where
      auxiliary i = do
        ex <- genRegEx i
        if producesEpsilon ex
          then
            auxiliary i
          else
            pure ex
  shrink Epsilon = []
  shrink (Literal c) = Epsilon : [Literal c' | c' <- shrink c]
  shrink (Range r) = Epsilon : [Range r' | r' <- shrink r]
  shrink (Star r) = [Epsilon, r] ++ [Star r' | r' <- shrink r]
  shrink (Concat r1 r2) =
    [r1, r2]
      ++ [Concat r1' r2 | r1' <- shrink r1]
      ++ [Concat r1 r2' | r2' <- shrink r2]
  shrink (Alter r1 r2) =
    [r1, r2]
      ++ [Alter r1' r2 | r1' <- shrink r1]
      ++ [Alter r1 r2' | r2' <- shrink r2]

genSpec :: Gen (DFALexerSpec Char Int T)
genSpec = sized $ \i ->
  dfaLexerSpec 0
    . zipWith (\j -> (toT j,)) [i ..]
    <$> replicateM i (arbitrary :: Gen (RegEx Char))
  where
    toT = T . Text.pack . show

testGenRegEx :: IO ()
testGenRegEx = do
  regex <- generate genSpec
  print regex
