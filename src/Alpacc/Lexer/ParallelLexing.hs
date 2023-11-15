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

data Comp t s k = Comp
  { maxTrans :: t,
    compositionMap :: Map (t, t) t,
    endsMap :: Map t (Set s),
    connectedMap :: Map t (Set t),
    tokenMap :: Map t (Set k)
  }
  deriving (Eq, Show)

type CompState t s k a = State (Comp t s k) a

addComposition :: (IsTransition t, Enum t, Ord k) => t -> t -> CompState t s k t
addComposition t t' = do
  comp <- get
  token_map <- gets tokenMap
  max_trans <- gets maxTrans
  composition_map <- gets compositionMap
  ends_map <- gets endsMap

  let new_max_trans = succ max_trans

  let t_token_set = Map.lookup t token_map
  let t_token_set' = Map.lookup t' token_map
  let new_token_map =
        flip (Map.insert new_max_trans) token_map
        $ fromMaybe Set.empty
        $ liftM2 Set.intersection t_token_set t_token_set'

  let new_ends = fromMaybe Set.empty $ Map.lookup t' ends_map
  let new_ends_map = Map.insert t' new_ends ends_map
  
  let new_composition_map = Map.insert (t, t') new_max_trans composition_map
  
  let new_comp =
        comp
        { maxTrans = new_max_trans
        , compositionMap = new_composition_map
        , tokenMap = new_token_map
        , endsMap = new_ends_map
        }

  let maybe_t = Map.lookup (t, t') composition_map
  put $ case maybe_t of
    Just _ -> comp
    Nothing -> new_comp

  return $ fromMaybe new_max_trans maybe_t

endsLookup :: (IsTransition t, IsState s) => t -> CompState t s k (Set s)
endsLookup t = do
  ends_map <- gets endsMap
  return $ fromMaybe Set.empty $ Map.lookup t ends_map

connectedLookup :: (IsTransition t, IsState s) => t -> CompState t s k (Set t)
connectedLookup t = do
  connected_map <- gets connectedMap
  return $ fromMaybe Set.empty $ Map.lookup t connected_map

transitions' :: (IsTransition t, IsState s) => DFA t s -> Map (s, t) s
transitions' =
  Map.mapKeys (second runIdentity)
  . fmap runIdentity
  . transitions

complete :: (IsTransition t, IsState s) => DFA t s -> Map (s, t) s
complete dfa = trans
  where
    trans = transitions' dfa

initEnds :: (IsTransition t, IsState s) => DFA t s -> Map t (Set s)
initEnds dfa =
  Map.unions
  $ auxiliary
  <$> _alphabet
  where
    _alphabet = Set.toList $ alphabet dfa
    _states = Set.toList $ states dfa
    _transitions = transitions' dfa

    auxiliary t =
      Map.singleton t
      $ Set.fromList
      $ mapMaybe ((`Map.lookup` _transitions) . (, t)) _states

initConnected :: (IsTransition t, IsState s) => DFA t s -> Map t (Set t)
initConnected dfa = Map.fromList $ auxiliary <$> _alphabet
  where
    _alphabet = Set.toList $ alphabet dfa
    _states = Set.toList $ states dfa
    _transitions = transitions' dfa

    auxiliary t =
      (t, )
      . Set.unions
      $ transitionsLookup
      <$> mapMaybe ((`Map.lookup` _transitions) . (, t)) _states

    transitionLookup s t =
      if (s, t) `Map.member` _transitions
      then Just t
      else Nothing
    
    transitionsLookup s =
      Set.fromList
      $ mapMaybe (transitionLookup s) _alphabet

initComp :: (IsTransition t, IsState s) => DFA t s -> Comp t s k
initComp dfa =
  Comp
  { maxTrans = max_trans
  , compositionMap = composition_map
  , connectedMap = connected_map
  , endsMap = ends_map
  , tokenMap = Map.empty
  }
  where
    max_trans = maximum $ alphabet dfa
    composition_map = Map.empty
    connected_map = initConnected dfa
    ends_map = initEnds dfa

