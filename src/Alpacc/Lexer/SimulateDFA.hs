module Alpacc.Lexer.SimulateDFA
  ( intDfaParallelLexer )
where

import Alpacc.Lexer.FSA
import Alpacc.Lexer.DFA
import Alpacc.Lexer.Encode
import Alpacc.Lexer.ParallelLexing
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map hiding (Map)
import Data.Set qualified as Set hiding (Set)
import Data.Maybe
import Data.Array.Base (IArray (..))
import Data.Array.Unboxed qualified as UArray hiding (UArray)
import Data.Array.Unboxed (UArray)
import Control.Monad

errorMessage :: String
errorMessage = "Error: Happend during Parallel Lexing genration, contact a maintainer."

type S = Int

initState :: S
initState = 1

deadState :: S
deadState = 0

-- | An extended endomorphism
data ExtEndo =
  ExtEndo !(UArray S S) !(UArray S Bool) deriving (Eq, Ord, Show)

instance Sim ExtEndo S where
  toState s (ExtEndo endo producing) = do
    let (a, b) = bounds endo
    unless (a <= s && s <= b) Nothing
    pure (producing UArray.! s, endo UArray.! s)

instance Semigroup ExtEndo where
  (ExtEndo a a') <> (ExtEndo b b') = ExtEndo c c'
    where
      c = UArray.array (0, numElements a - 1)
        $ map auxiliary [0..(numElements a - 1)]
      c' = UArray.array (0, numElements a' - 1)
        $ map auxiliary' [0..(numElements a' - 1)]
      auxiliary i = (i, b UArray.! (a UArray.! i))
      auxiliary' i = (i, b' UArray.! (a UArray.! i))

endomorphismTable ::
  (Ord t, Ord s, Ord k) =>
  ParallelDFALexer t s k ->
  (ParallelDFALexer t S k, Map t ExtEndo)
endomorphismTable lexer' =
  (lexer, )
  $ Map.fromList
  $ map statesFromChar
  $ Set.toList
  $ alphabet dfa
  where
    lexer = enumerateParLexer initState lexer'
    dfa = fsa $ parDFALexer lexer
    produces_set = producesToken lexer
    states_list = Set.toList $ states dfa
    states_size = Set.size $ states dfa
    statesFromChar t = (t, ExtEndo ss bs)
      where
        ss = auxiliary $ fromMaybe deadState . uncurry (transition lexer) . (, t)
        bs = auxiliary $ (`Set.member` produces_set) . (, t)
        auxiliary f =
          UArray.array (deadState, states_size + 1)
          $ zip [deadState..states_size + 1]
          $ f <$> states_list

dfaParallelLexer ::
  (Enum t, Bounded t, Ord t, Ord s, Ord k) =>
  ParallelDFALexer t s k ->
  ParallelLexer t (ExtEndoData k)
dfaParallelLexer lexer' =
  parallelLexer lexer toEndo
  where
    (lexer, table) = endomorphismTable lexer'
    toEndo = (table Map.!)

intDfaParallelLexer ::
  (Enum t, Bounded t, Ord t, Ord s, Ord k) =>
  Map (Maybe k) Int ->
  ParallelDFALexer t s k ->
  Either String (IntParallelLexer t)
intDfaParallelLexer mapping =
  intParallelLexer mapping . dfaParallelLexer
