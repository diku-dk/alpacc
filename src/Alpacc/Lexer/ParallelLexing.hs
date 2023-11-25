module Alpacc.Lexer.ParallelLexing
  ( complete
  , Comp (..)
  )
where

import Alpacc.Util
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
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap hiding (IntMap)
import Data.Function
import Data.Tuple
import Data.Bifunctor
import Data.Functor.Identity

type IntLexer t = DFALexer Int Int t
type Endomorphism = Array Int Int

data EndoMap =
  EndoMap (IntMap Endomorphism) (Map Endomorphism Int)
  deriving (Show, Eq, Ord)

insert :: EndoMap -> Int -> Endomorphism -> EndoMap
insert (EndoMap map' map'') i e =
  EndoMap (IntMap.insert i e map') (Map.insert e i map'')

fromList :: [(Int, Endomorphism)] -> EndoMap
fromList ls =
  EndoMap (IntMap.fromList ls) (Map.fromList $ swap <$> ls)

unsafeLookup :: EndoMap -> Int -> Endomorphism
unsafeLookup (EndoMap map' _) i = map' IntMap.! i

safeLookupR :: EndoMap -> Endomorphism -> Maybe Int
safeLookupR (EndoMap _ map') e = Map.lookup e map'

data Comp =
  Comp
  { compositions :: Map (Int, Int) Int
  , connected :: IntMap IntSet
  , endomorphisms :: EndoMap
  , maxKeyComp :: Int
  } deriving (Show, Eq, Ord)

type CompState a = State Comp a

compose :: Endomorphism -> Endomorphism -> Endomorphism
compose a b =
  Array.array (0, length a - 1)
  $ map (\i -> (i, b Array.! (a Array.! i))) [0..(length a - 1)]

compose' :: Int -> Int -> CompState ()
compose' a b = do
  comp <- get
  _endomorphisms <- gets endomorphisms
  _compositions <- gets compositions
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
        comp { compositions = new_comps
             , endomorphisms = new_endos
             , maxKeyComp = new_endo_key
             , connected = new_connected}

  put $
    if (a, b) `Map.member` _compositions then
      comp
    else
      new_comp
  
initEndomorphisms :: Ord t => Int -> IntLexer t -> EndoMap
initEndomorphisms dead_state lexer = table
  where
    _states = states $ fsa lexer
    last_index = Set.size _states - 1
    toArray = Array.array (0, last_index) . zip [0..last_index]
    table =
      fromList
      $ Map.toList
      $ toArray <$> parallelLexingTable dead_state lexer

parallelLexingTable :: (IsState s, IsTransition t, Enum s) => s -> DFALexer t s k -> Map t [s]
parallelLexingTable dead_state lexer = table
  where
    dfa = fsa lexer
    _transitions = transitions' dfa
    _states = states dfa
    _alphabet = alphabet dfa
    tableLookUp key = fromMaybe dead_state $ Map.lookup key _transitions
    statesFromChar a =
      (a,) $
        map (\b -> tableLookUp (b, a)) $
          Set.toList _states
    table = Map.fromList $ map statesFromChar $ Set.toList _alphabet

initConnected :: Ord t => IntLexer t -> IntMap IntSet
initConnected lexer = IntMap.fromList $ auxiliary <$> _alphabet
  where
    dfa = fsa lexer
    _alphabet = Set.toList $ alphabet dfa
    _states = Set.toList $ states dfa
    _transitions = transitions' dfa

    auxiliary t =
      (t, )
      . IntSet.unions
      $ transitionsLookup
      <$> mapMaybe ((`Map.lookup` _transitions) . (, t)) _states

    transitionLookup s t =
      if (s, t) `Map.member` _transitions
      then Just t
      else Nothing
    
    transitionsLookup s =
      IntSet.fromList
      $ mapMaybe (transitionLookup s) _alphabet

addCompositions :: Int -> IntSet -> CompState ()
addCompositions t t_set = do
  mapM_ (compose' t) $ IntSet.toList t_set

complete' :: CompState ()
complete' = do
  _connected <- gets connected
  old_compositions <- gets compositions
  mapM_ (uncurry addCompositions) $ IntMap.toList _connected
  new_compositions <- gets compositions
  when (old_compositions /= new_compositions) complete'

stateMap :: Int -> EndoMap -> IntMap Int
stateMap init' (EndoMap map' _) = (Array.! init') <$> map'

addDeadState :: IntLexer t -> (Int, IntLexer t)
addDeadState old_lexer = (dead_state, new_lexer)
  where
    old_dfa = fsa old_lexer
    old_states = states old_dfa
    dead_state = succ $ maximum old_states
    new_states = Set.insert dead_state old_states
    new_dfa = old_dfa { states = new_states }
    new_lexer = old_lexer { fsa = new_dfa }

complete :: (Show t, Ord t) => IntLexer t -> (Set Int, IntMap Int, Map (Int, Int) Int, Map ((Int, Int), Int) t, Int, Int, Int)
complete lexer' =
  ( initialLoopSet lexer
  , stateMap initial_state $ endomorphisms final_comp 
  , compositions final_comp
  , tokenMap lexer
  , dead_state
  , succ $ maxKeyComp final_comp
  , succ $ succ $ maxKeyComp final_comp)
  where
    final_comp = execState complete' comp
    max_key = maximum . alphabet $ fsa lexer
    (dead_state, lexer) = addDeadState lexer'
    initial_state = initial $ fsa lexer
    _endomorphisms = initEndomorphisms dead_state lexer
    
    comp =
      Comp
      { connected = initConnected lexer
      , endomorphisms = _endomorphisms
      , compositions = Map.empty
      , maxKeyComp = max_key}
