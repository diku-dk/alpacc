module Alpacc.Lexer.ParallelLexing where

import Alpacc.Lexer.FSA
import Alpacc.Lexer.DFA
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Set (Set)
-- import Data.Set qualified as Set hiding (Set)
-- import Data.Maybe
import Data.Functor.Identity
import Data.Bifunctor
-- import Data.List qualified as List

data Comp s = Comp
  { maxEdge :: Integer,
    compositionMap :: Map (Integer, Integer) Integer,
    graph :: Map s (Set s),
    edgeMap :: Map (s, s) (Set Integer)
  }
  deriving (Eq, Show)

type CompState s = State (Comp s) ()

transitions' :: (IsState s, IsTransition t) => DFA t s -> Map (s, t) s
transitions' =
  Map.mapKeys (second runIdentity)
  . fmap runIdentity
  . transitions

complete :: (IsState s) => DFA Integer s -> Map (s, Integer) s
complete dfa = trans
  where
    -- alpha = alphabet dfa
    trans = transitions' dfa

initGraph :: (IsState s) => DFA Integer s -> Map s (Set s)
initGraph dfa = Map.empty
  where
    _alphabet = alphabet dfa
    _states = states dfa
    _transitions = transitions dfa

    -- temp = Set.map id _states

initComp :: (IsState s) => DFA Integer s -> Comp s
initComp dfa =
  Comp
  { maxEdge = max_edge
  , compositionMap = composition_map
  , graph = Map.empty
  , edgeMap = Map.empty
  }
  where
    max_edge = maximum $ alphabet dfa
    composition_map = Map.empty

-- newEdges :: (IsState s) => s -> CompState s
-- newEdges s = 
