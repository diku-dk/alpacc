module Alpacc.Lexer.NFA
  ( NFA,
    RegEx (..),
    Transition (..),
    isTransition,
    fromTransition,
    fromRegExToNFA,
    statesTransitions,
    epsilonClosure,
  )
where

import Alpacc.Lexer.FSA
import Alpacc.Lexer.RegularExpression
import Control.Monad.State
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map hiding (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)

type NFA t s = FSA Set (Transition t) s

data Transition t = Trans t | Eps deriving (Show, Eq, Ord)

isTransition :: Transition t -> Bool
isTransition (Trans _) = True
isTransition Eps = False

fromTransition :: Transition t -> t
fromTransition (Trans t) = t
fromTransition Eps = error "Can not unpack Eps."

instance Functor Transition where
  fmap f (Trans t) = Trans (f t)
  fmap _ Eps = Eps

instance OrdMap Transition where
  omap f (Trans t) = Trans (f t)
  omap _ Eps = Eps

toSet :: Transition a -> Set (Transition a)
toSet t@(Trans _) = Set.singleton t
toSet Eps = Set.empty

initNFA :: (Ord s, Enum s) => s -> NFA t s
initNFA start_state =
  FSA
    { states = Set.fromList [start_state, succ start_state],
      alphabet = Set.empty,
      transitions = Map.empty,
      initial = start_state,
      accepting = Set.singleton $ succ start_state
    }

newState :: (Ord s, Ord t, Enum s) => State (NFA t s) s
newState = do
  nfa <- get
  let max_state = Set.findMax $ states nfa
  let new_max_state = succ max_state
  let new_states' = Set.insert new_max_state $ states nfa
  put $ nfa {states = new_states'}
  return new_max_state

newTransition :: (Ord s, Ord t) => s -> Transition t -> s -> State (NFA t s) ()
newTransition s c s' = do
  nfa <- get
  let trans = transitions nfa
  let key = (s, c)
  let new_trans =
        if key `Map.member` trans
          then Map.adjust (Set.insert s') key trans
          else Map.insert key (Set.singleton s') trans
  let new_alph = toSet c `Set.union` alphabet nfa
  put $ nfa {transitions = new_trans, alphabet = new_alph}

newTransitions :: (Ord s, Ord t, Foldable f, Enum s) => s -> f t -> s -> State (NFA t s) ()
newTransitions z ts' = auxiliary z (toList ts')
  where
    auxiliary s [] s' = newTransition s Eps s'
    auxiliary s [t] s' = newTransition s (Trans t) s'
    auxiliary s (t : ts) s'' = do
      s' <- newState
      newTransition s (Trans t) s'
      auxiliary s' ts s''

auxRegExToNFA :: (Ord s, Ord t, Foldable f, Enum s) => s -> s -> RegEx (f t) -> State (NFA t s) ()
auxRegExToNFA s s' Epsilon = do
  newTransition s Eps s'
auxRegExToNFA s s' (Literal cs) = do
  newTransitions s cs s'
auxRegExToNFA s s'' (Concat a b) = do
  s' <- newState
  auxRegExToNFA s s' a
  auxRegExToNFA s' s'' b
auxRegExToNFA s s'' (Star a) = do
  s' <- newState
  newTransition s Eps s'
  newTransition s' Eps s''
  auxRegExToNFA s' s' a
auxRegExToNFA s s' (Range range) = do
  mapM_ (\cs -> newTransitions s cs s') range
auxRegExToNFA s s' alter@(Alter _ _) = do
  mapM_ (auxRegExToNFA s s') $ findAlters alter
  where
    findAlters (Alter a b) = findAlters a ++ findAlters b
    findAlters regex = [regex]

regExToNFA :: (Ord s, Ord t, Enum s) => RegEx (NonEmpty t) -> State (NFA t s) ()
regExToNFA regex = do
  nfa <- get
  let (s, s') = (initial nfa, accepting nfa)
  let accept_list = toList s'
  mapM_ (\_s -> auxRegExToNFA s _s regex) accept_list

fromRegExToNFA :: (Ord s, Ord t, Enum s) => s -> RegEx (NonEmpty t) -> NFA t s
fromRegExToNFA start_state regex = execState (regExToNFA regex) init_nfa
  where
    init_nfa = initNFA start_state

stateTransitions :: (Ord s, Ord t) => Transition t -> s -> State (NFA t s) (Set s)
stateTransitions c s = do
  nfa <- get
  let trans = transitions nfa
  let eps_map = Map.filterWithKey (\k _ -> isSymbolTransition k) trans
  pure . Set.unions $ toList eps_map
  where
    isSymbolTransition (s', c') = s == s' && c == c'

epsilonTransitions :: (Ord s, Ord t) => s -> State (NFA t s) (Set s)
epsilonTransitions = stateTransitions Eps

statesTransitions :: (Ord s, Ord t) => Set s -> Transition t -> State (NFA t s) (Set s)
statesTransitions set c = Set.unions <$> mapM (stateTransitions c) (toList set)

epsilonClosure :: (Ord s, Ord t) => Set s -> State (NFA t s) (Set s)
epsilonClosure set = do
  new_set <- Set.unions <$> mapM epsilonTransitions (toList set)
  let set' = new_set `Set.union` set
  if set == set'
    then pure set'
    else epsilonClosure set'
