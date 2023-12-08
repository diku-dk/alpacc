module Alpacc.Lexer.ParallelLexing
  ( Endomorphism
  -- , parallelLexer
  , ParallelLexer (..)
  )
where

import Alpacc.Lexer.FSA
import Alpacc.Lexer.DFA
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet hiding (IntSet)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Maybe
import Data.Array (Array)
import Data.Array qualified as Array hiding (Array)
import Data.List qualified as List
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap hiding (IntMap)
import Data.Function
import Data.Bifunctor
import Data.Word

type State = Int
type Endomorphism = Array State State

data ParallelLexer t k =
  ParallelLexer
  { compositions :: Map (Int, Int) Int
  , endomorphisms :: Map t Endomorphism
  , transitionsToKey :: Map t Int
  , endomorphismsToStates :: Map Int Int
  , tokenMap :: Map Int k
  , initialState :: Int
  , identity :: Int
  , deadEndomorphismKey :: Int
  , deadState :: Int
  , stateSize :: Int
  , endomorphismsSize :: Int
  , acceptingStates :: Set Int
  , deadEndomorphism :: Endomorphism
  } deriving (Show, Eq, Ord)

data ModLexer t k =
  ModLexer
  { dfaLexer :: DFALexer t State k
  , theDeadState :: State
  } deriving (Show, Eq, Ord)

statesToKeysMap :: Ord s => Set s -> Map s State
statesToKeysMap =
  Map.fromList
  . flip zip [(0 :: State)..]
  . Set.toAscList

addDeadState :: (IsState s, IsTransition t, Enum s) => DFALexer t s k -> (s, DFALexer t s k)
addDeadState old_lexer = (dead_state, new_lexer)
  where
    old_dfa = fsa old_lexer
    old_states = states old_dfa
    dead_state = succ $ maximum old_states
    new_states = Set.insert dead_state old_states
    new_dfa = old_dfa { states = new_states }
    new_lexer = old_lexer { fsa = new_dfa }    

toModLexer :: (IsState s, IsTransition t) => DFALexer t s k -> ModLexer t k
toModLexer lexer =
  ModLexer
  { dfaLexer = new_lexer
  , theDeadState = dead_state
  }
  where
    states_to_keys_map = statesToKeysMap $ states $ fsa lexer
    stateToKey = (states_to_keys_map Map.!)
    (dead_state, new_lexer) =
      addDeadState
      $ fsaLexerSecond stateToKey lexer

compose :: Endomorphism -> Endomorphism -> Endomorphism
compose  a b =
  Array.array (0, length a - 1)
  $ map auxiliary [0..(length a - 1)]
  where
    auxiliary i = (i, b Array.! (a Array.! i))

safeCompose :: State -> State -> Set State -> Endomorphism -> Endomorphism -> Endomorphism
safeCompose initial_state dead_state accept a b =
  Array.array (0, length a - 1)
  $ map auxiliary [0..(length a - 1)]
  where
    auxiliary i = (i, m)
      where
        j = a Array.! i
        k = b Array.! j
        m =
          if j `Set.member` accept && k == dead_state then
            initial_state
          else
            k

endomorphismTable :: (IsTransition t, Ord k) => ModLexer t k -> Map t Endomorphism
endomorphismTable lexer =
  Map.fromList
  $ map statesFromChar
  $ Set.toList _alphabet
  where
    dfa = fsa $ dfaLexer lexer
    dead_state = theDeadState lexer
    _transitions = transitions' dfa
    _states = states dfa
    _alphabet = alphabet dfa
    last_index = maximum _states
    toArray = Array.array (minBound, last_index) . zip [minBound ..last_index]
    tableLookUp key =
      fromMaybe dead_state
      $ Map.lookup key _transitions
    statesFromChar t =
      (t,)
      $ toArray
      $ map (tableLookUp . (, t))
      $ Set.toAscList _states

findInitialTrans :: IsTransition t => ModLexer t k -> Set t
findInitialTrans lexer =
  Set.fromList
  $ mapMaybe auxiliary
  $ Set.toList _alphabet
  where
    dfa = fsa $ dfaLexer lexer
    _transitions = transitions' dfa
    _initial = initial dfa
    _alphabet = alphabet dfa
    auxiliary t =
      if (_initial, t) `Map.member` _transitions
      then Just t
      else Nothing

findFinalTrans :: IsTransition t => ModLexer t k -> Set t
findFinalTrans lexer =
  Set.fromList
  $ fmap snd
  $ Map.keys
  $ Map.filter (`Set.member` _accepting) _transitions   
  where
    dfa = fsa $ dfaLexer lexer
    _transitions = transitions' dfa
    _accepting = accepting dfa
    _alphabet = alphabet dfa

findLoopTrans :: IsTransition t => ModLexer t k -> Map t (Set t)
findLoopTrans lexer = Map.fromList $ zip _final (repeat _init)
  where
    _init = findInitialTrans lexer
    _final = Set.toList (findFinalTrans lexer)

connectedTable :: IsTransition t => ModLexer t k -> Map t (Set t)
connectedTable lexer =
  Map.unionWith Set.union loop_trans
  $ Map.fromList
  $ auxiliary <$> _alphabet
  where
    dfa = fsa $ dfaLexer lexer
    _alphabet = Set.toList $ alphabet dfa
    _states = Set.toAscList $ states dfa
    _transitions = transitions' dfa

    loop_trans = findLoopTrans lexer

    auxiliary t =
      (t, )
      $ Set.unions
      $ transitionsLookup
      <$> mapMaybe ((`Map.lookup` _transitions) . (, t)) _states

    transitionLookup s t =
      if (s, t) `Map.member` _transitions
      then Just t
      else Nothing
    
    transitionsLookup s =
      Set.fromList
      $ mapMaybe (transitionLookup s) _alphabet

pairEndomorphisms :: (IsTransition t, Ord k) => ModLexer t k -> Map (t, t) Endomorphism
pairEndomorphisms lexer =
  Map.fromList
  $ concat
  $ Map.elems
  $ Map.mapWithKey toPair connected_table
  where
    dead_state = theDeadState lexer
    initial_state = initial $ fsa $ dfaLexer lexer
    accept = accepting $ fsa $ dfaLexer lexer
    connected_table = connectedTable lexer
    endomorphism_table = endomorphismTable lexer

    toConn = (connected_table Map.!)
    toEndo = (endomorphism_table Map.!)

    safeCompose' =
      on (safeCompose initial_state dead_state accept) toEndo

    toPair t =
      fmap (\t' -> ((t, t'), safeCompose' t t'))
      . Set.toList

pairConnected :: IsTransition t => ModLexer t k -> Map (t, t) (Set (t, t))
pairConnected lexer =
  Map.fromList
  $ concat
  $ Map.elems
  $ Map.mapWithKey toPair connected_table
  where
    connected_table = connectedTable lexer
    toConn = (connected_table Map.!)

    auxiliary t t' =
      ((t, t'), )
      $ Set.fromList
      $ fmap (t',)
      $ Set.toList
      $ toConn t'

    toPair t =
      fmap (auxiliary t)
      . Set.toList

connEndos :: (IsTransition t, Ord k) => ModLexer t k -> Map Endomorphism (Set Endomorphism)
connEndos lexer =
  Map.unionsWith Set.union
  $ auxiliary
  <$> Map.toList connected_table
  where
    connected_table = pairConnected lexer
    endomorphism_table = pairEndomorphisms lexer

    auxiliary (t, t_set) = Map.singleton (toEndo t) (Set.map toEndo t_set)
    toEndo = (endomorphism_table Map.!)

-- parallelLexer :: (Enum s, IsState s, IsTransition t, Ord k) => DFALexer t s k -> ParallelLexer t k
-- parallelLexer lexer' =
--   ParallelLexer
--   { compositions = compositions' final_comp
--   , endomorphisms = table 
--   , identity = _id
--   , tokenMap = token_map
--   , transitionsToKey = trans_to_endo
--   , endomorphismsToStates = endomorphisms_to_states
--   , initialState = initial_state_key
--   , deadEndomorphismKey = dead_endo_key
--   , deadState = dead_state_key
--   , stateSize = state_size
--   , endomorphismsSize = IntSet.size endo_keys
--   , acceptingStates = accepting_states
--   , deadEndomorphism = dead_endomorphism
--   }
--   where
--     endo_keys = IntMap.keysSet map'
--     dead_endomorphism =
--       Array.array (0, last_index)
--       $ map (, dead_state_key) [0..state_size - 1]
-- 
--     dead_endo_key = succ $ succ $ maxKeyComp final_comp
--     state_size = Set.size $ states $ fsa lexer
--     
--     dead_state_key = states_to_keys Map.! dead_state
--     accepting_states = Set.map (states_to_keys Map.!) $ accepting $ fsa lexer
--     token_map = Map.mapKeys (states_to_keys Map.!) $ terminalMap lexer
--     initial_state_key = states_to_keys Map.! initial_state
--     endomorphisms_to_states =
--       endomorphismsToStatesMap initial_state_key endo_map
--     states_to_keys = statesToKeysMap $ states $ fsa lexer
--     endo_map@(EndoMap map' _) = endomorphisms' final_comp
--     (_id, final_comp) =
--       runState (addIdentity last_index)
--       $ execState complete comp
--     last_index = pred $ Set.size $ states $ fsa lexer
--     max_key = maximum trans_to_endo
--     (dead_state, lexer) = addDeadState lexer'
--     initial_state = initial $ fsa lexer
--     trans_to_endo = initTransToEndo table _endomorphisms'
--     table = parallelLexingTable dead_state lexer
--     _endomorphisms' = initEndomorphisms table
--     
--     comp =
--       Comp
--       { connected = initConnected lexer trans_to_endo
--       , endomorphisms' = _endomorphisms'
--       , compositions' = Map.empty
--       , maxKeyComp = max_key
--       , deadStateComp = dead_state_key
--       , initialStateComp = initial_state_key
--       , acceptComp = IntSet.fromList $ Set.toList accepting_states }
