module Alpacc.Lexer.ParallelLexing
  ( Endomorphism
  , parallelLexer
  , ParallelLexer (..)
  )
where

import Alpacc.Lexer.FSA
import Alpacc.Lexer.DFA
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map hiding (Map)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap hiding (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet hiding (IntSet)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Maybe
import Data.Array (Array)
import Data.Array qualified as Array hiding (Array)
import Data.Bifunctor (Bifunctor (..))
import Data.Tuple (swap)
import Control.Monad.State
import Alpacc.Debug
import Data.List qualified as List

type S = Int
type E = Int
type Endomorphism = Array S S

data ParallelLexer t k =
  ParallelLexer
  { compositions :: Map (E, E) E
  , endomorphisms :: Map t E
  , tokenEndomorphism :: Set (S, t) 
  , endomorphismsToStates :: Map E S
  , tokenMap :: Map S k
  , identity :: E
  , stateSize :: Int
  , endomorphismsSize :: Int
  , acceptingStates :: Set S
  } deriving (Show, Eq, Ord)

data EndoCtx =
  EndoCtx
  { comps :: Map (E, E) E
  , endoMap :: Map Endomorphism E
  , inverseEndoMap :: IntMap Endomorphism
  , connectedMap :: IntMap IntSet
  , maxE :: E
  } deriving (Show, Eq, Ord)

type EndoState = State EndoCtx

errorMessage :: String
errorMessage = "Error: Happend during Parallel Lexing genration, contact a maintainer."

endoInsert :: E -> Endomorphism -> EndoState ()
endoInsert e endo = do
  inv_map <- gets inverseEndoMap
  let new_inv_map = IntMap.insert e endo inv_map

  _map <- gets endoMap
  let new_map = Map.insert endo e _map
  
  modify $
    \s ->
      s { inverseEndoMap = new_inv_map
        , endoMap = new_map }

eLookup :: E -> EndoState (Maybe Endomorphism)
eLookup e = do
  inv_map <- gets inverseEndoMap
  return $ IntMap.lookup e inv_map

connectedLookup :: E -> EndoState (Maybe IntSet)
connectedLookup e = do
  _map <- gets connectedMap
  return $ IntMap.lookup e _map

connectedUpdate :: E -> IntSet -> EndoState ()
connectedUpdate e e_set = do
  _map <- gets connectedMap
  if e `IntMap.member` _map
  then
    let new_map = IntMap.insertWith IntSet.union e e_set _map
    in modify $ \s -> s { connectedMap = new_map }
  else
    let new_map = IntMap.insert e e_set _map
    in modify $ \s -> s { connectedMap = new_map }

connectedUpdateAll :: IntMap IntSet -> EndoState ()
connectedUpdateAll _map =
  mapM_ (uncurry connectedUpdate) $ IntMap.assocs _map

insertComposition :: E -> E -> E -> EndoState ()
insertComposition e e' e'' = do
  _map <- gets comps
  let new_map = Map.insert (e, e') e'' _map
  modify $ \s -> s { comps = new_map }

connectedDiff :: IntSet -> IntSet -> Maybe IntSet
connectedDiff a b = if IntSet.null c then Nothing else Just c
  where
    c = IntSet.difference a b

preSets :: E -> E -> EndoState (IntMap IntSet)
preSets e'' e = do
  _map <- gets connectedMap
  let set = IntSet.singleton e''
  let new_map =
        IntMap.fromList
        $ fmap (,set)
        $ IntMap.keys
        $ IntMap.filter (e `IntSet.member`) _map
  return $ IntMap.unionWith IntSet.union new_map _map
  -- return $ IntMap.differenceWith connectedDiff new_map _map

postSets :: E -> E -> EndoState (IntMap IntSet)
postSets e'' e' = do
  _map <- gets connectedMap
  e_set <- fromMaybe IntSet.empty <$> connectedLookup e'
  let new_map = IntMap.singleton e'' e_set
  return $ IntMap.unionWith IntSet.union new_map _map
  -- return $ IntMap.differenceWith connectedDiff new_map _map
  
endomorphismLookup :: Endomorphism -> EndoState (Maybe E)
endomorphismLookup endomorphism = do
  _map <- gets endoMap
  return $ Map.lookup endomorphism _map

compose :: Endomorphism -> Endomorphism -> Endomorphism
compose a b =
  Array.array (0, length a - 1)
  $ map auxiliary [0..(length a - 1)]
  where
    auxiliary i = (i, b Array.! (a Array.! i))

endoNext :: EndoState E
endoNext = do
  max_e <- gets maxE
  let new_max_e = succ max_e
  modify $ \s -> s { maxE = new_max_e }
  return new_max_e

endoCompose :: E -> E -> EndoState (Maybe (IntMap IntSet))
endoCompose e e' = do
  _comps <- gets comps
  if (e, e') `Map.member` _comps
    then return Nothing
    else result
  where
    result = do
      maybe_endo <- eLookup e
      maybe_endo' <- eLookup e'
      case (maybe_endo, maybe_endo') of
        (Just endo, Just endo') -> do
          let endo'' = endo `compose` endo'
          maybe_e'' <- endomorphismLookup endo''
          e'' <- maybe endoNext return maybe_e''
          endoInsert e'' endo''
          insertComposition e e' e''
          pre_sets <- preSets e'' e
          post_sets <- postSets e'' e'
          let new_sets =
                IntMap.unionWith IntSet.union pre_sets post_sets
          return $
            if IntMap.null new_sets
            then Nothing
            else Just new_sets
        _ -> error "This should never happend."

composeMany :: E -> IntSet -> EndoState (IntMap IntSet)
composeMany e e_set = do
  e_list <- mapM (endoCompose e) $ IntSet.toList e_set
  return
    $ IntMap.unionsWith IntSet.union
    $ catMaybes e_list

composeAll :: IntMap IntSet -> EndoState (IntMap IntSet)
composeAll _map = do
  maps <- mapM (uncurry composeMany) $ IntMap.assocs _map
  return $ IntMap.unionsWith IntSet.union maps

endoCompositionsTable :: IntMap IntSet -> EndoState ()
endoCompositionsTable _map = do
  prev_e <- gets maxE
  new_map <- composeAll _map
  next_e <- gets maxE
  if prev_e == next_e
    then return ()
    else connectedUpdateAll new_map *> endoCompositionsTable new_map

compositionsTable ::
  (Enum t, Bounded t, IsTransition t, Ord k) =>
  ParallelDFALexer t S k ->
  EndoCtx
compositionsTable lexer =
  execState (endoCompositionsTable connected_map) ctx 
  where
    ctx = initEndoCtx lexer
    connected_map = connectedMap ctx

endomorphismTable ::
  (Enum t, Bounded t, IsTransition t, Ord k) =>
  ParallelDFALexer t S k ->
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

connectedTable :: IsTransition t => ParallelDFALexer t S k -> Map t (Set t)
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

initEndoCtx ::
  (Enum t, Bounded t, IsTransition t, Ord k) =>
  ParallelDFALexer t S k ->
  EndoCtx
initEndoCtx lexer =
  EndoCtx
  { comps = Map.empty
  , endoMap = endo_to_e
  , inverseEndoMap = e_to_endo
  , connectedMap = connected_table
  , maxE = maximum endo_to_e
  }
  where
    endo_table = endomorphismTable lexer
    
    e_to_endo =
      IntMap.fromList
      $ zip [0 :: E ..]
      $ List.nub
      $ Map.elems endo_table
    endo_to_e =
      Map.fromList
      $ swap
      <$> IntMap.assocs e_to_endo
    endoToE = (endo_to_e Map.!)
    t_to_e = endoToE <$> endo_table
    tToE = (t_to_e Map.!)
    connected_table =
      IntMap.unionsWith IntSet.union
      $ fmap (uncurry IntMap.singleton . first tToE)
      $ Map.toList
      $ IntSet.fromList . fmap tToE . Set.toList
      <$> connectedTable lexer

toStateMap :: S -> Map Endomorphism E -> Map E S
toStateMap initial_state =
  Map.fromList
  . fmap (swap . first (Array.! initial_state))
  . Map.toList

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
  ParallelDFALexer t S k ->
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
  ParallelDFALexer t S k ->
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
  ParallelDFALexer t S k ->
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
    (EndoCtx
      { comps = _compositions'
      , endoMap = to_endo'
      }) = compositionsTable lexer
    vec_dead = deadEndomorphism lexer
    vec_identity = identityEndomorphism lexer
    to_endo =
      Map.insert vec_dead _dead
      $ Map.insert vec_identity _identity to_endo'
    _identity =
      case Map.lookup vec_identity to_endo' of
        Nothing -> succ $ maximum to_endo'
        Just a -> a
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
      $ addIdentity _identity _compositions'
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
