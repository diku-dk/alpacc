module Alpacc.Lexer.ParallelLexing
  ( Endomorphism
  , parallelLexer
  , ParallelLexer (..)
  )
where

import Alpacc.Util (fixedPointIterate)
import Alpacc.Lexer.FSA
import Alpacc.Lexer.DFA
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Maybe
import Data.Array (Array)
import Data.Array qualified as Array hiding (Array)
import Data.Function (on)
import Data.Bifunctor (Bifunctor (..))
import Data.Tuple (swap)
import Data.Tuple.Extra (both)
import Alpacc.Debug (debug)

type State = Int
type Endo = Int
type Endomorphism = Array State State

data ParallelLexer t k =
  ParallelLexer
  { compositions :: Map (Endo, Endo) Endo
  , endomorphisms :: Map t Endomorphism
  , transitionsToEndos :: Map (t, t) Endo
  , endomorphismsToStates :: Map Endo State
  , tokenMap :: Map State k
  , initialState :: State
  , identity :: Endo
  , deadEndo :: Endo
  , deadEndomorphism :: Endomorphism
  , deadState :: State
  , stateSize :: Int
  , endomorphismsSize :: Int
  , acceptingStates :: Set State
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
            j

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
    first_index = minimum _states
    last_index = maximum _states
    toArray = Array.array (first_index, last_index) . zip [first_index..last_index]
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
findLoopTrans lexer = Map.fromList $ map (, _init) _final
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

deadEndomorphism' :: ModLexer t k -> Endomorphism
deadEndomorphism' lexer =
  Array.array (first_state, last_state)
  $ (,dead_state) <$> [first_state..last_state]
  where
    _states = states $ fsa $ dfaLexer lexer
    first_state = minimum _states
    last_state = maximum _states
    dead_state = theDeadState lexer

identityEndomorphism :: ModLexer t k -> Endomorphism
identityEndomorphism lexer =
  Array.array (first_state, last_state)
  $ zip [first_state..last_state] [first_state..last_state]
  where
    _states = states $ fsa $ dfaLexer lexer
    first_state = minimum _states
    last_state = maximum _states

addDeadAndIdentity :: ModLexer t k -> Map Endomorphism (Set Endomorphism) -> Map Endomorphism (Set Endomorphism)
addDeadAndIdentity lexer _map =
  Map.insert _id endo_set
  $ Map.insert dead endo_set map'
  where
    map' = Set.union new_endos <$> _map
    endo_set = endomorphismSet map'
    dead = deadEndomorphism' lexer
    _id = identityEndomorphism lexer
    new_endos = Set.fromList [dead, _id]

initConnected :: (IsTransition t, Ord k) => ModLexer t k -> Map Endomorphism (Set Endomorphism)
initConnected lexer =
  addDeadAndIdentity lexer
  $ Map.unionsWith Set.union
  $ auxiliary
  <$> Map.toList connected_table
  where
    connected_table = pairConnected lexer
    endomorphism_table = pairEndomorphisms lexer

    auxiliary (t, t_set) = Map.singleton (toEndo t) (Set.map toEndo t_set)
    toEndo = (endomorphism_table Map.!)

newEndoConn ::
  Map Endomorphism (Set Endomorphism) ->
  Endomorphism ->
  Set Endomorphism ->
  Map Endomorphism (Set Endomorphism)
newEndoConn conn_endos endo endo_set =
  Map.unionsWith Set.union
  $ Set.map toMap endo_set
  where
    toConn = (conn_endos Map.!)
    toMap endo' =
      Map.singleton comp (toConn endo') `Map.union` new_map
      where
        comp = endo `compose` endo'
        set = Set.singleton comp
        new_map =
          Map.unions
          $ fmap (`Map.singleton` set)
          $ Map.keys
          $ Map.filter (endo `Set.member`) conn_endos

newEndoConns ::
  Map Endomorphism (Set Endomorphism) ->
  Map Endomorphism (Set Endomorphism)
newEndoConns conn_endos =
  Map.unionWith Set.union conn_endos
  $ Map.unionsWith Set.union
  $ Map.mapWithKey (newEndoConn conn_endos) conn_endos

connected ::
  Map Endomorphism (Set Endomorphism) ->
  Map Endomorphism (Set Endomorphism)
connected = fixedPointIterate (/=) newEndoConns

compositionsTable :: Map Endomorphism (Set Endomorphism) -> Map (Endomorphism, Endomorphism) Endomorphism
compositionsTable _connected =
  Map.fromList
  $ concat
  $ Map.mapWithKey auxiliary _connected
  where
    toMap e e' = ((e, e'), e `compose` e')
    auxiliary e = Set.toList . Set.map (toMap e)

endomorphismSet :: Map Endomorphism (Set Endomorphism) -> Set Endomorphism
endomorphismSet _connected =
  Set.union (Map.keysSet _connected)
  $ Set.unions _connected

enumerateEndomorphisms :: Map Endomorphism (Set Endomorphism) -> Map Endomorphism Endo
enumerateEndomorphisms =
  Map.fromList
  . flip zip [0..]
  . Set.toList
  . endomorphismSet

toStateMap :: State -> Map Endomorphism Endo -> Map Endo State
toStateMap initial_state =
  Map.fromList
  . fmap (swap . first (Array.! initial_state))
  . Map.toList

endoCompositions :: (Endomorphism -> Endo) -> Map (Endomorphism, Endomorphism) Endomorphism -> Map (Endo, Endo) Endo
endoCompositions toEndo comps =
  Map.mapKeys (both toEndo)
  $ toEndo <$> comps

parallelLexer :: (Enum s, IsState s, IsTransition t, Ord k) => DFALexer t s k -> ParallelLexer t k
parallelLexer lexer' = 
  ParallelLexer
  { compositions = _compositions
  , endomorphisms = _endomorphisms
  , identity = _identity
  , tokenMap = token_map
  , transitionsToEndos = _transitions_to_endo
  , endomorphismsToStates = to_state
  , initialState = initial_state
  , deadEndo = dead_endo
  , deadEndomorphism = dead_endomorphism
  , deadState = dead_state
  , stateSize = state_size
  , endomorphismsSize = endo_size
  , acceptingStates = accept_states
  }
  where
    lexer = toModLexer lexer'
    accept_states = accepting $ fsa $ dfaLexer lexer
    endo_size = Map.size to_endo
    state_size = Set.size $ states $ fsa $ dfaLexer lexer
    _connected = connected $ initConnected lexer
    to_endo = enumerateEndomorphisms _connected
    toEndo = (to_endo Map.!)
    _endomorphisms = endomorphismTable lexer
    _compositions = endoCompositions toEndo $ compositionsTable _connected
    _identity = toEndo $ identityEndomorphism lexer
    dead_endomorphism = deadEndomorphism' lexer
    dead_endo = toEndo dead_endomorphism
    token_map = terminalMap $ dfaLexer lexer
    _transitions_to_endo = toEndo <$> pairEndomorphisms lexer
    initial_state = initial $ fsa $ dfaLexer lexer
    dead_state = theDeadState lexer
    to_state = toStateMap initial_state to_endo
