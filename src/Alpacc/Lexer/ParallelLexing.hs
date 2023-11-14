module Alpacc.Lexer.ParallelLexing where

import Alpacc.Lexer.FSA
import Alpacc.Lexer.DFA
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Maybe
import Data.Functor.Identity
import Data.Bifunctor
-- import Data.List qualified as List

data Comp s = Comp
  { maxEdge :: Integer,
    compositionMap :: Map (Integer, Integer) Integer,
    connectionMap :: Map s (Set s),
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

initConnectionMap :: (IsState s) => DFA Integer s -> Map s (Set s)
initConnectionMap dfa =
  Map.unions
  $ auxiliary <$> _states
  where
    _alphabet = Set.toList $ alphabet dfa
    _states = Set.toList $ states dfa
    _transitions = transitions' dfa

    auxiliary s =
      Map.singleton s
      $ Set.fromList
      $ mapMaybe ((`Map.lookup` _transitions) . (s, )) _alphabet

initEdgeMap :: (IsState s) => DFA Integer s -> Map (s, s) (Set Integer)
initEdgeMap dfa =
  Map.unions
  $ auxiliary <$> _states
  where
    _alphabet = Set.toList $ alphabet dfa
    _states = Set.toList $ states dfa
    _transitions = transitions' dfa

    auxiliary s =
      Map.unionsWith Set.union
      $ mapMaybe (edgeLookup s) _alphabet

    edgeLookup s t = do
      s' <- Map.lookup (s, t) _transitions
      return $ Map.singleton (s, s') $ Set.singleton t

initComp :: (IsState s) => DFA Integer s -> Comp s
initComp dfa =
  Comp
  { maxEdge = max_edge
  , compositionMap = composition_map
  , connectionMap = connection_map
  , edgeMap = edge_map
  }
  where
    max_edge = maximum $ alphabet dfa
    composition_map = Map.empty
    connection_map = initConnectionMap dfa
    edge_map = initEdgeMap dfa

-- newEdges :: (IsState s) => s -> CompState s
-- newEdges s = 
