module Alpacc.Lexer.ParallelLexing
  ( Endomorphism
  , parallelLexer
  , ParallelLexer (..)
  )
where

import Alpacc.Util (fixedPointIterate)
import Alpacc.Lexer.FSA
import Alpacc.Lexer.DFA
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map hiding (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Maybe
import Data.Array (Array)
import Data.Array qualified as Array hiding (Array)
import Data.Bifunctor (Bifunctor (..))
import Data.Tuple (swap)
import Data.Tuple.Extra (both)

type State = Int
type Endo = Int
type Endomorphism = Array State State

data ParallelLexer t k =
  ParallelLexer
  { compositions :: Map (Endo, Endo) Endo
  , endomorphisms :: Map t Endo
  , tokenEndomorphism :: Set (State, t) 
  , endomorphismsToStates :: Map Endo State
  , tokenMap :: Map State k
  , identity :: Endo
  , stateSize :: Int
  , endomorphismsSize :: Int
  , acceptingStates :: Set State
  } deriving (Show, Eq, Ord)

compose :: Endomorphism -> Endomorphism -> Endomorphism
compose a b =
  Array.array (0, length a - 1)
  $ map auxiliary [0..(length a - 1)]
  where
    auxiliary i = (i, b Array.! (a Array.! i))

endomorphismTable ::
  (Enum t, Bounded t, IsTransition t, Ord k) =>
  ParallelDFALexer t State k ->
  Map t Endomorphism
endomorphismTable lexer =
  Map.fromList
  $ map statesFromChar
  $ Set.toList _alphabet
  where
    dfa = fsa $ parDFALexer lexer
    dead_state = deadState lexer
    _transitions = transitions' dfa
    _states = states dfa
    _alphabet = alphabet dfa
    first_index = minimum _states
    last_index = maximum _states
    toArray =
      Array.array (first_index, last_index)
      . zip [first_index..last_index]
    tableLookUp key =
      fromMaybe dead_state
      $ Map.lookup key _transitions
    statesFromChar t =
      (t,)
      $ toArray
      $ map (tableLookUp . (, t))
      $ Set.toAscList _states

connectedTable :: IsTransition t => ParallelDFALexer t State k -> Map t (Set t)
connectedTable lexer =
  Map.fromList
  $ auxiliary <$> _alphabet
  where
    dfa = fsa $ parDFALexer lexer
    _alphabet = Set.toList $ alphabet dfa
    _states = Set.toAscList $ states dfa
    _transitions = transitions' dfa

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


invertMap :: Ord t => Map t (Set t) -> Map t (Set t)
invertMap mapping =
  Map.unionsWith Set.union
  $ toMap <$> Map.toList mapping
  where
    toMap (t, t_set) =
      Map.unionsWith Set.union
      $ flip Map.singleton (Set.singleton t) <$> Set.toList t_set

initConnected ::
  (Enum t, Bounded t, IsTransition t, Ord k) =>
  ParallelDFALexer t State k ->
  Map Endomorphism (Set Endomorphism)
initConnected lexer =
  Map.unionWith Set.union temp
  $ Map.unionsWith Set.union
  $ mapMaybe auxiliary
  $ Map.toList connected_table
  where
    connected_table = connectedTable lexer
    inverted_connected_table = invertMap connected_table
    temp =
      Map.unionsWith Set.union
      $ mapMaybe auxiliary
      $ Map.toList inverted_connected_table
    endomorphism_table = endomorphismTable lexer

    auxiliary (t, t_set) = do
      e <- toEndo t
      let t_set' =
            Set.fromList
            $ mapMaybe toEndo
            $ Set.toList t_set
      return $ Map.singleton e t_set'

    toEndo t = Map.lookup t endomorphism_table    

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
      Map.unionWith Set.union new_map
      $ Map.singleton comp (toConn endo')
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

compositionsTable ::
  Map Endomorphism (Set Endomorphism) ->
  Map (Endomorphism, Endomorphism) Endomorphism
compositionsTable _connected =
  Map.fromList
  $ concat
  $ Map.mapWithKey auxiliary _connected
  where
    toMap e e' = ((e, e'), e `compose` e')
    auxiliary e = Set.toList . Set.map (toMap e)

endomorphismSet ::
  Map Endomorphism (Set Endomorphism) ->
  Set Endomorphism
endomorphismSet _connected =
  Set.union (Map.keysSet _connected)
  $ Set.unions _connected

enumerateEndomorphisms ::
  Map Endomorphism (Set Endomorphism) ->
  Map Endomorphism Endo
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

endoCompositions ::
  (Endomorphism -> Endo) ->
  Map (Endomorphism, Endomorphism) Endomorphism ->
  Map (Endo, Endo) Endo
endoCompositions toEndo comps =
  Map.mapKeys (both toEndo)
  $ toEndo <$> comps

endosInTable :: Ord t => Map (t, t) t -> Set t
endosInTable table = endos
  where
    endos =
      Set.union (Set.fromList right)
      $ Set.union (Set.fromList left)
      $ Set.fromList
      $ Map.elems table
    (left, right) =
      unzip
      $ Map.keys table

addIdentity :: Ord t => t -> Map (t, t) t -> Map (t, t) t
addIdentity identity_endo table =
  Map.union right_endos
  $ Map.union table left_endos
  where
    left_endos =
      Map.fromList $ (\q -> ((identity_endo, q), q)) <$> endos
    right_endos =
      Map.fromList $ (\q -> ((q, identity_endo), q)) <$> endos
    endos =
      Set.toList
      $ Set.insert identity_endo
      $ endosInTable table

addDead :: Ord t => t -> Map (t, t) t -> Map (t, t) t
addDead dead_endo table =
  Map.union table deads
  where
    deads =
      Map.fromList [((q, q'), dead_endo) | q <- endos, q' <- endos]
    endos =
      Set.toList
      $ Set.insert dead_endo
      $ endosInTable table

deadEndomorphism ::
  (IsTransition t, Enum t, Bounded t, Ord k) =>
  ParallelDFALexer t State k ->
  Endomorphism
deadEndomorphism lexer =
  Array.array (first_state, last_state)
  $ (,dead_state) <$> [first_state..last_state]
  where
    _states = states $ fsa $ parDFALexer lexer
    first_state = minimum _states
    last_state = maximum _states
    dead_state = deadState lexer

identityEndomorphism ::
  (IsTransition t, Enum t, Bounded t, Ord k) =>
  ParallelDFALexer t State k ->
  Endomorphism
identityEndomorphism lexer =
  Array.array (first_state, last_state)
  $ zip [first_state..last_state] [first_state..last_state]
  where
    _states = states $ fsa $ parDFALexer lexer
    first_state = minimum _states
    last_state = maximum _states


parallelLexer ::
  (IsTransition t, Enum t, Bounded t, Ord k) =>
  ParallelDFALexer t State k ->
  ParallelLexer t k
parallelLexer lexer = 
  ParallelLexer
  { compositions = _compositions
  , endomorphisms = _transitions_to_endo
  , identity = _identity
  , tokenMap = token_map
  , endomorphismsToStates = to_state
  , stateSize = state_size
  , endomorphismsSize = endo_size
  , acceptingStates = accept_states
  , tokenEndomorphism = producesToken lexer
  }
  where
    accept_states = accepting $ fsa $ parDFALexer lexer
    endo_size = Set.size $ endosInTable _compositions
    state_size = Set.size $ states $ fsa $ parDFALexer lexer
    _connected = connected $ initConnected lexer
    to_endo' = enumerateEndomorphisms _connected
    vec_dead = deadEndomorphism lexer
    to_endo =
      Map.insert vec_dead _dead
      $ Map.insert (identityEndomorphism lexer) _identity to_endo'
    _identity = succ $ maximum to_endo'
    _dead =
      case Map.lookup vec_dead to_endo' of
        Nothing -> succ _identity
        Just a -> a
    toEndo x =
      case Map.lookup x to_endo of
           Nothing -> error "Error: Happend during Parallel Lexing genration, contact a maintainer."
           Just a -> a
    _compositions =
      addDead _dead
      $ addIdentity _identity
      $ endoCompositions toEndo
      $ compositionsTable _connected
    token_map = terminalMap $ parDFALexer lexer
    _alphabet = Set.toList $ alphabet $ fsa $ parDFALexer lexer
    _dead_transitions =
      Map.fromList
      $ map (,_dead) [minBound..maxBound]
    _transitions_to_endo =
      flip Map.union _dead_transitions
      $ toEndo
      <$> endomorphismTable lexer
    initial_state = initial $ fsa $ parDFALexer lexer
    dead_state = deadState lexer
    to_state =
      Map.insert _dead dead_state
      $ Map.insert _identity initial_state
      $ toStateMap initial_state to_endo
