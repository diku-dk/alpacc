module Alpacc.Lexer.NFA
  ( NFA (..),
    RegEx (..),
    mkNFA,
    initNFA
  )
where

import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Alpacc.Lexer.RegularExpression

data NFA t s = NFA
  { states' :: Set s,
    transitions' :: Map (s, Maybe Char) (Set s),
    initial' :: s,
    alphabet' :: Set Char,
    accepting' :: Set s,
    terminalMap' :: Map ((s, s), Char) (Set t),
    finalTerminalStates' :: Map t (Set s),
    initialTerminalStates' :: Map t (Set s),
    continueTerminalStates' :: Map t (Set s)
  }
  deriving (Show)

initNFA :: (Ord s, Enum s) => s -> NFA t s
initNFA start_state =
  NFA
    { states' = Set.fromList [start_state, succ start_state],
      alphabet' = Set.empty,
      transitions' = Map.empty,
      initial' = start_state,
      accepting' = Set.singleton $ succ start_state,
      terminalMap' = Map.empty,
      finalTerminalStates' = Map.empty,
      initialTerminalStates' = Map.empty,
      continueTerminalStates' = Map.empty
    }

newState :: (Ord s, Enum s) => State (NFA t s) s
newState = do
  nfa <- get
  let max_state = Set.findMax $ states' nfa
  let new_max_state = succ max_state
  let new_states' = Set.insert new_max_state $ states' nfa
  put
    ( nfa
        { states' = new_states'
        }
    )
  return new_max_state

newTransition :: (Ord s) => s -> Maybe Char -> s -> State (NFA t s) ()
newTransition s c s' = do
  nfa <- get
  let trans = transitions' nfa
  let key = (s, c)
  let new_trans =
        if key `Map.member` trans
          then Map.adjust (Set.insert s') key trans
          else Map.insert key (Set.singleton s') trans
  let new_alph = Set.fromList (maybeToList c) `Set.union` alphabet' nfa
  put $ nfa {transitions' = new_trans, alphabet' = new_alph}

markToken :: (Ord t, Ord s) => (Set ((s, s), Char), Set s) -> t -> s -> s -> State (NFA t s) ()
markToken (set, continue_set') t s s' = do
  nfa <- get

  let continue_sets = continueTerminalStates' nfa
  let continue_set = fromMaybe Set.empty $ Map.lookup t continue_sets
  let new_continue_set = continue_set' `Set.union` continue_set
  let new_continue_sets = Map.insert t new_continue_set continue_sets

  let initial_sets = initialTerminalStates' nfa
  let initial_set = fromMaybe Set.empty $ Map.lookup t initial_sets
  let new_initial_set = Set.insert s initial_set
  let new_initial_sets = Map.insert t new_initial_set initial_sets

  let final_sets = finalTerminalStates' nfa
  let final_set = fromMaybe Set.empty $ Map.lookup t final_sets
  let new_final_set = Set.insert s' final_set
  let new_final_sets = Map.insert t new_final_set final_sets

  let new_map = Map.fromList $ (,Set.singleton t) <$> Set.toList set
  let _map = terminalMap' nfa

  put (nfa {
    terminalMap' = Map.unionWith Set.union new_map _map,
    finalTerminalStates' = new_final_sets,
    initialTerminalStates' = new_initial_sets,
    continueTerminalStates' = new_continue_sets
    })

epsilon :: Maybe a
epsilon = Nothing

combineMkNFAReturn :: Ord s => (Set ((s, s), Char), Set s) -> (Set ((s, s), Char), Set s) -> (Set ((s, s), Char), Set s)
combineMkNFAReturn (a, b) (a', b') = (Set.union a a', Set.union b b')

mkNFA' :: (Ord t, Ord s, Enum s) => s -> s -> RegEx t -> State (NFA t s) (Set ((s, s), Char), Set s)
mkNFA' s s' Epsilon = do
  newTransition s epsilon s'
  return (Set.empty, Set.empty)
mkNFA' s s' (Literal c) = do
  newTransition s (Just c) s'
  return (Set.singleton ((s, s'), c), Set.empty)
mkNFA' s s'' (Concat a b) = do
  s' <- newState
  new <- mkNFA' s s' a
  new' <- mkNFA' s' s'' b
  return $ combineMkNFAReturn new new'
mkNFA' s s'''' (Alter a b) = do
  s' <- newState
  s'' <- newState
  s''' <- newState
  newTransition s epsilon s'
  newTransition s epsilon s''
  newTransition s''' epsilon s''''
  new <- mkNFA' s' s''' a
  new' <- mkNFA' s'' s''' b
  return $ combineMkNFAReturn new new'
mkNFA' s s'' (Star a) = do
  s' <- newState
  newTransition s epsilon s'
  newTransition s epsilon s''
  new <- mkNFA' s' s a
  return $ second (Set.insert s') new
mkNFA' s s''' (Token t a) = do
  s' <- newState
  s'' <- newState
  newTransition s epsilon s'
  newTransition s'' epsilon s'''
  new <- mkNFA' s' s'' a
  markToken new t s' s''
  return new
mkNFA' s s' (Range range) = do
  let chars = concatMap toChars range
  mapM_ (\c -> newTransition s (Just c) s') chars
  return (Set.fromList $ ((s, s'),) <$> chars, Set.empty)
  where
    toChars (Right (a, b)) = [a .. b]
    toChars (Left a) = [a]

mkNFA :: (Ord t, Show s, Ord s, Enum s) => RegEx t -> State (NFA t s) ()
mkNFA regex = do
  nfa <- get
  let (s, s') = (initial' nfa, accepting' nfa)
  let accept_list = toList s'
  mapM_ (\_s -> mkNFA' s _s regex) accept_list