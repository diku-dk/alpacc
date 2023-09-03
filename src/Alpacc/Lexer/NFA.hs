module Alpacc.Lexer.NFA
  ( NFA,
    RegEx (..),
    mkNFA,
    initNFA,
    Transition (..),
    lexerNFA,
    isTransition,
    fromTransition,
    fromRegExToNFA
  )
where

import Control.Monad.State
import Data.Foldable (Foldable (..))
import Data.Map qualified as Map hiding (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Alpacc.Lexer.RegularExpression
import Alpacc.Lexer.FSA
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)

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

toSet :: Transition a -> Set a
toSet (Trans t) = Set.singleton t
toSet Eps = Set.empty

type NFA t s = FSA Set Transition t s
type NFALexer t s k = Lexer Set Transition t s k

initNFA :: (IsState s, Enum s) => s -> NFA t s
initNFA start_state =
  FSA
    { states = Set.fromList [start_state, succ start_state],
      alphabet = Set.empty,
      transitions = Map.empty,
      initial = start_state,
      accepting = Set.singleton $ succ start_state
    }

newState :: (IsState s, IsTransition t, Enum s) => State (NFA t s) s
newState = do
  nfa <- get
  let max_state = Set.findMax $ states nfa
  let new_max_state = succ max_state
  let new_states' = Set.insert new_max_state $ states nfa
  put
    ( nfa
        { states = new_states'
        }
    )
  return new_max_state

newTransition :: (IsState s, IsTransition t) => s -> Transition t -> s -> State (NFA t s) ()
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

newTransitions :: (IsState s, IsTransition t, Foldable f, Enum s) => s -> f t -> s -> State (NFA t s) ()
newTransitions z ts' = auxiliary z (toList ts')
  where
    auxiliary s [] s' = newTransition s Eps s'
    auxiliary s [t] s' = newTransition s (Trans t) s'
    auxiliary s (t:ts) s'' = do
      s' <- newState
      newTransition s (Trans t) s'
      auxiliary s' ts s''

mkNFA' :: (IsState s, IsTransition t, Foldable f, Enum s) => s -> s -> RegEx (f t) -> State (NFA t s) ()
mkNFA' s s' Epsilon = do
  newTransition s Eps s'
mkNFA' s s' (Literal cs) = do
  newTransitions s cs s'
mkNFA' s s'' (Concat a b) = do
  s' <- newState
  mkNFA' s s' a
  mkNFA' s' s'' b
mkNFA' s s'' (Star a) = do
  s' <- newState
  newTransition s Eps s'
  newTransition s' Eps s''
  mkNFA' s' s' a
mkNFA' s s' (Range range) = do
  mapM_ (\cs -> newTransitions s cs s') range
mkNFA' s s' alter@(Alter _ _) = do
  mapM_ (mkNFA' s s') $ findAlters alter
  where
    findAlters (Alter a b) = findAlters a ++ findAlters b
    findAlters regex = [regex]

mkNFA :: (IsState s, IsTransition t, Enum s) => RegEx (NonEmpty t) -> State (NFA t s) ()
mkNFA regex = do
  nfa <- get
  let (s, s') = (initial nfa, accepting nfa)
  let accept_list = toList s'
  mapM_ (\_s -> mkNFA' s _s regex) accept_list


fromRegExToNFA :: (IsState s, IsTransition t, Enum s) => s -> RegEx (NonEmpty t) -> NFA t s
fromRegExToNFA start_state regex = execState (mkNFA regex) init_nfa
  where
    init_nfa = initNFA start_state

lexerNFA :: (IsTransition t, IsState s, Enum s, Ord k) => s -> Map k (NFA t s) -> NFALexer t s k
lexerNFA start_state nfa_map' = 
  Lexer {
    fsa = nfa,
    finalMap = final_map,
    terminalMap = terminal_map,
    deadState = Nothing
  }
  where
    nfa_map = reenumerateFSAsMap start_state nfa_map'
    final_map = accepting <$> nfa_map
    toMap k ((s, t), set) =
      Map.unionsWith Set.union
      $ (\s' -> Map.singleton ((s, s'), t) (Set.singleton k)) <$> toList set
    toMaps k = 
      Map.unionsWith Set.union
      . fmap (toMap k)
    terminal_map =
      Map.unionsWith Set.union
      $ Map.mapWithKey toMaps
      $ Map.toList . transitions <$> nfa_map
    nfas = Map.elems nfa_map
    initials = Set.fromList $ initial <$> nfas

    new_states' = Set.unions $ states <$> nfas
    new_initial = succ $ maximum new_states'
    new_states = Set.insert new_initial new_states'
    new_alphabet = Set.unions $ alphabet <$> nfas
    new_transitions =
      Map.insert (new_initial, Eps) initials
      $ Map.unionsWith Set.union
      $ transitions <$> nfas
    new_accepting = Set.unions $ accepting <$> nfas

    nfa = FSA {
      states = new_states,
      alphabet = new_alphabet,
      transitions = new_transitions,
      accepting = new_accepting,
      initial = new_initial
    }