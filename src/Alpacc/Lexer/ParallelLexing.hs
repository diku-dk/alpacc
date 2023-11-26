module Alpacc.Lexer.ParallelLexing
  ( Endomorphism
  , parallelLexer
  , ParallelLexer (..)
  )
where

import Alpacc.Lexer.FSA
import Alpacc.Lexer.DFA
import Control.Monad.State

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

type Endomorphism = Array Int Int

data EndoMap =
  EndoMap (IntMap Endomorphism) (Map Endomorphism Int)
  deriving (Show, Eq, Ord)

data Comp =
  Comp
  { compositions' :: Map (Int, Int) Int
  , connected :: IntMap IntSet
  , endomorphisms' :: EndoMap
  , maxKeyComp :: Int
  } deriving (Show, Eq, Ord)

type CompState a = State Comp a

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

insert :: EndoMap -> Int -> Endomorphism -> EndoMap
insert (EndoMap map' map'') i e =
  EndoMap (IntMap.insert i e map') (Map.insert e i map'')

unsafeLookup :: EndoMap -> Int -> Endomorphism
unsafeLookup (EndoMap map' _) i = map' IntMap.! i

safeLookupR :: EndoMap -> Endomorphism -> Maybe Int
safeLookupR (EndoMap _ map') e = Map.lookup e map'

compose :: Endomorphism -> Endomorphism -> Endomorphism
compose a b =
  Array.array (0, length a - 1)
  $ map (\i -> (i, b Array.! (a Array.! i))) [0..(length a - 1)]

compose' :: Int -> Int -> CompState ()
compose' a b = do
  comp <- get
  _endomorphisms <- gets endomorphisms'
  _compositions <- gets compositions'
  _connected <- gets connected
  max_key <- gets maxKeyComp
  
  let new_endo = on compose (unsafeLookup _endomorphisms) a b
  let new_endo_key = fromMaybe (succ max_key) $ safeLookupR _endomorphisms new_endo
  let new_endos = insert _endomorphisms new_endo_key new_endo
  let new_comps = Map.insert (a, b) new_endo_key _compositions
  
  let connections = fromMaybe IntSet.empty $ IntMap.lookup b _connected
  let new_connected =
        if new_endo_key `IntMap.member` _connected then
          IntMap.adjust (IntSet.intersection connections) new_endo_key _connected
        else
          IntMap.insert  new_endo_key connections _connected

  let new_comp =
        comp { compositions' = new_comps
             , endomorphisms' = new_endos
             , maxKeyComp = new_endo_key
             , connected = new_connected}

  put $
    if (a, b) `Map.member` _compositions then
      comp
    else
      new_comp

addIdentity :: Int -> CompState Int
addIdentity last_index = do
  comp <- get
  let _identity =
        Array.array (0, last_index)
        $ [(i, i) | i <- [0..last_index]]
  _endomorphisms <- gets endomorphisms'
  max_key <- gets maxKeyComp
  let new_id_key = succ max_key
  let new_endos = insert _endomorphisms new_id_key _identity
  let new_comp' =
        comp
        { maxKeyComp = new_id_key
        , endomorphisms' = new_endos}
  
  let (new_comp, id_key) =
        case safeLookupR _endomorphisms _identity of
          Just key -> (comp, key)
          Nothing -> (new_comp', new_id_key)
  put new_comp

  (EndoMap map' _) <- gets endomorphisms'

  mapM_ (compose' id_key) $ IntMap.keys map'
  mapM_ (`compose'` id_key) $ IntMap.keys map'
  
  return id_key

initEndomorphisms :: (Ord t) => Map t Endomorphism -> EndoMap
initEndomorphisms endos = EndoMap map' map''
  where
    map' = IntMap.fromList $ zip [0..] $ List.nub $ Map.elems endos
    map'' = Map.fromList $ flip zip [0..] $ List.nub $ Map.elems endos

initTransToEndo :: (Ord t) => Map t Endomorphism -> EndoMap -> Map t Int 
initTransToEndo endos (EndoMap _ map') =
  (map' Map.!) <$> endos

statesToKeysMap :: Ord s => Set s -> Map s Int
statesToKeysMap =
  Map.fromList
  . flip zip [(0 :: Int)..]
  . Set.toAscList

parallelLexingTable :: (IsState s, IsTransition t, Enum s, Ord k) => s -> DFALexer t s k -> Map t Endomorphism
parallelLexingTable dead_state' lexer = table
  where
    dfa = fsa lexer
    state_to_int = statesToKeysMap _states
    stateToInt = (state_to_int Map.!)
    _transitions =
      Map.mapKeys (first stateToInt)
      $ stateToInt <$> transitions' dfa
    dead_state = state_to_int Map.! dead_state'
    _states = states dfa
    _alphabet = alphabet dfa
    last_index = Set.size _states - 1
    toArray = Array.array (0, last_index) . zip [0..last_index]
    tableLookUp key =
      fromMaybe dead_state
      $ Map.lookup key _transitions
    statesFromChar a =
      (a,) $
        toArray $
        map (\b -> tableLookUp (snd b, a)) $
          Map.toAscList state_to_int
    table = Map.fromList $ map statesFromChar $ Set.toList _alphabet

initConnected :: (IsState s, IsTransition t) => DFALexer t s k -> Map t Int -> IntMap IntSet
initConnected lexer trans_to_int =
  IntMap.fromList
  $ auxiliary <$> _alphabet
  where
    dfa = fsa lexer
    _alphabet = Set.toList $ alphabet dfa
    _states = Set.toAscList $ states dfa
    _transitions = transitions' dfa

    transToInt = (trans_to_int Map.!)
    
    auxiliary t =
      (transToInt t, )
      . IntSet.unions
      $ transitionsLookup
      <$> mapMaybe ((`Map.lookup` _transitions) . (, t)) _states

    transitionLookup s t =
      if (s, t) `Map.member` _transitions
      then Just (transToInt t)
      else Nothing
    
    transitionsLookup s =
      IntSet.fromList
      $ mapMaybe (transitionLookup s) _alphabet

addCompositions :: Int -> IntSet -> CompState ()
addCompositions t t_set = do
  mapM_ (compose' t) $ IntSet.toList t_set

complete :: CompState ()
complete = do
  _connected <- gets connected
  old_compositions' <- gets compositions'
  mapM_ (uncurry addCompositions) $ IntMap.toList _connected
  new_compositions' <- gets compositions'
  when (old_compositions' /= new_compositions') complete

endomorphismsToStatesMap :: Int -> EndoMap -> Map Int Int
endomorphismsToStatesMap init' (EndoMap map' _) =
  Map.fromList
  $ IntMap.toList
  $ (Array.! init') <$> map'

addDeadState :: (IsState s, IsTransition t, Enum s) => DFALexer t s k -> (s, DFALexer t s k)
addDeadState old_lexer = (dead_state, new_lexer)
  where
    old_dfa = fsa old_lexer
    old_states = states old_dfa
    dead_state = succ $ maximum old_states
    new_states = Set.insert dead_state old_states
    new_dfa = old_dfa { states = new_states }
    new_lexer = old_lexer { fsa = new_dfa }

parallelLexer :: (Enum s, IsState s, IsTransition t, Ord k) => DFALexer t s k -> ParallelLexer t k
parallelLexer lexer' =
  ParallelLexer
  { compositions = compositions' final_comp
  , endomorphisms = table 
  , identity = _id
  , tokenMap = token_map
  , transitionsToKey = trans_to_endo
  , endomorphismsToStates = endomorphisms_to_states
  , initialState = initial_state_key
  , deadEndomorphismKey = succ $ maxKeyComp final_comp
  , deadState = states_to_keys Map.! dead_state
  , stateSize = state_size
  , endomorphismsSize = IntSet.size $ IntMap.keysSet map'
  , acceptingStates = accepting_states
  , deadEndomorphism = dead_endomorphism
  }
  where
    state_size = Set.size $ states $ fsa lexer
    dead_endomorphism =
      Array.array (0, last_index)
      $ map (, states_to_keys Map.! dead_state) [0..state_size - 1]
    accepting_states = Set.map (states_to_keys Map.!) $ accepting $ fsa lexer
    token_map = Map.mapKeys (states_to_keys Map.!) $ terminalMap lexer
    initial_state_key = states_to_keys Map.! initial_state
    endomorphisms_to_states =
      endomorphismsToStatesMap initial_state_key endo_map
    states_to_keys = statesToKeysMap $ states $ fsa lexer
    endo_map@(EndoMap map' _) = endomorphisms' final_comp
    (_id, final_comp) =
      runState (addIdentity last_index)
      $ execState complete comp
    last_index = pred $ Set.size $ states $ fsa lexer
    max_key = maximum trans_to_endo
    (dead_state, lexer) = addDeadState lexer'
    initial_state = initial $ fsa lexer
    trans_to_endo = initTransToEndo table _endomorphisms'
    table = parallelLexingTable dead_state lexer
    _endomorphisms' = initEndomorphisms table
    
    comp =
      Comp
      { connected = initConnected lexer trans_to_endo
      , endomorphisms' = _endomorphisms'
      , compositions' = Map.empty
      , maxKeyComp = max_key}
