module Alpacc.Lexer.DFAParallelLexer
  ( dfaParallelLexer,
    intDfaParallelLexer,
  )
where

import Alpacc.Lexer.DFA
import Alpacc.Lexer.Encode
import Alpacc.Lexer.FSA
import Alpacc.Lexer.ParallelLexing
import Data.Array.Base (IArray (..))
import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as UArray hiding (UArray)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map hiding (Map)
import Data.Maybe
import Data.Set qualified as Set hiding (Set)

errorMessage :: String
errorMessage = "Error: Happend during Parallel Lexing genration, contact a maintainer."

data Endomorphism
  = Endomorphism
      {-# UNPACK #-} !(UArray S S)
      {-# UNPACK #-} !(UArray S Bool)
  deriving (Eq, Ord, Show)

type S = Int

deadState :: S
deadState = 0

initState :: S
initState = 1

endomorphismTable ::
  (Enum t, Bounded t, Ord t, Ord k) =>
  DFALexer t S k ->
  Map t Endomorphism
endomorphismTable lexer =
  Map.fromList $
    map statesFromChar $
      Set.toList _alphabet
  where
    dfa = fsa lexer
    produces_set = producesToken lexer
    _transitions = transitions' dfa
    _states = states dfa
    _alphabet = alphabet dfa
    first_index = deadState
    last_index = maximum _states
    states_list = deadState : Set.toAscList _states
    tableLookUp key =
      fromMaybe deadState $
        Map.lookup key _transitions
    statesFromChar t = (t, Endomorphism ss bs)
      where
        ss =
          UArray.array (first_index, last_index) $
            zip [first_index .. last_index] $
              map (tableLookUp . (,t)) states_list
        bs =
          UArray.array (first_index, last_index) $
            zip [first_index .. last_index] $
              map ((`Set.member` produces_set) . (,t)) states_list

instance Semigroup Endomorphism where
  (Endomorphism a a') <> (Endomorphism b b') = Endomorphism c c'
    where
      c =
        UArray.array (0, numElements a - 1) $
          map auxiliary [0 .. (numElements a - 1)]
      c' =
        UArray.array (0, numElements a' - 1) $
          map auxiliary' [0 .. (numElements a' - 1)]
      auxiliary i = (i, b UArray.! (a UArray.! i))
      auxiliary' i = (i, b' UArray.! (a UArray.! i))

instance Sim Endomorphism S where
  toState s endo =
    if a <= s && s <= b
      then (producing UArray.! s, endo' UArray.! s)
      else error errorMessage
    where
      (Endomorphism endo' producing) = endo
      (a, b) = bounds endo'

dfaParallelLexer ::
  (Ord t, Ord s, Enum t, Bounded t, Ord k) =>
  DFALexer t s k ->
  ParallelLexer t (EndoData k)
dfaParallelLexer lexer' = parallelLexer lexer endo_table
  where
    lexer = enumerateLexer initState lexer'
    endo_table = endomorphismTable lexer

intDfaParallelLexer ::
  (Ord t, Ord s, Enum t, Bounded t, Ord k) =>
  Map (Maybe k) Int ->
  DFALexer t s k ->
  Either String (IntParallelLexer t)
intDfaParallelLexer m = intParallelLexer m . dfaParallelLexer
