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

addComposition :: (IsState s, IsTransition t, Enum t, Ord k) => t -> t -> CompState t s k ()
addComposition t t' = do
  comp <- get
  token_map <- gets tokenMap
  max_trans <- gets maxTrans
  composition_map <- gets compositionMap
  ends_map <- gets endsMap
  connected_map <- gets connectedMap

  let new_max_trans = succ max_trans

  new_connected <- connectedLookup t'
  let new_connected_map =
        Map.insert new_max_trans new_connected connected_map
  
  let t_token_set = Map.lookup t token_map
  let t_token_set' = Map.lookup t' token_map
  let new_token_map =
        flip (Map.insert new_max_trans) token_map
        $ fromMaybe Set.empty
        $ liftM2 Set.intersection t_token_set t_token_set'

  new_ends <- endsLookup t'
  let new_ends_map = Map.insert t' new_ends ends_map
  
  let new_composition_map =
        Map.insert (t, t') new_max_trans composition_map
  
  let new_comp =
        comp
        { maxTrans = new_max_trans
        , compositionMap = new_composition_map
        , tokenMap = new_token_map
        , endsMap = new_ends_map
        , connectedMap = new_connected_map
        }

  let maybe_t = Map.lookup (t, t') composition_map
  put $ case maybe_t of
    Just _ -> comp
    Nothing -> new_comp

addCompositions :: (IsState s, IsTransition t, Enum t, Ord k) => t -> CompState t s k ()
addCompositions t = do
  connections <- fmap (t, ) . Set.toList <$> connectedLookup t
  mapM_ (uncurry addComposition) connections

compositionsStep :: (IsState s, IsTransition t, Enum t, Ord k) => CompState t s k ()
compositionsStep = do
  trans <- gets (Map.keys . connectedMap)
  mapM_ addCompositions trans

compositions :: (IsState s, IsTransition t, Enum t, Ord k) => CompState t s k ()
compositions = do
  curr_composition_map <- gets compositionMap
  compositionsStep
  next_composition_map <- gets compositionMap
  if curr_composition_map == next_composition_map
    then return ()
    else compositions
  

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

complete :: (IsTransition t, IsState s, Ord k) => DFALexer t s k -> Comp t s k
complete lexer = comp
  where
    comp = initComp lexer

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

initToken :: (IsTransition t, IsState s, Ord k) => DFALexer t s k -> Map t (Set k)
initToken lexer = token_map
  where
    _alphabet = Set.toList $ alphabet $ fsa lexer
    token_map =
      Map.unionsWith Set.union
      $ uncurry Map.singleton
      . first (runIdentity . snd)
      <$> Map.toList (terminalMap lexer)

initComp :: (IsTransition t, IsState s, Ord k) => DFALexer t s k -> Comp t s k
initComp lexer =
  Comp
  { maxTrans = max_trans
  , compositionMap = composition_map
  , connectedMap = connected_map
  , endsMap = ends_map
  , tokenMap = token_map
  }
  where
    dfa = fsa lexer
    max_trans = maximum $ alphabet dfa
    composition_map = Map.empty
    connected_map = initConnected dfa
    ends_map = initEnds dfa
    token_map = initToken lexer

