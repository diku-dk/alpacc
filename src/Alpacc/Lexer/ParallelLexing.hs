module Alpacc.Lexer.ParallelLexing
  ( complete
  , Comp (..)
  )
where

import Alpacc.Lexer.FSA
import Alpacc.Lexer.DFA
import Control.Monad.State
import Control.Monad
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

type Endomorphism = Array Int Int

data EndoBimap =
  EndoBimap (IntMap Endomorphism) (Map Endomorphism Int)
  deriving (Show, Eq)

insert :: EndoBimap -> Int -> Endomorphism -> EndoBimap
insert (EndoBimap map' map'') i e =
  EndoBimap (IntMap.insert i e map') (Map.insert e i map'')

fromList :: [(Int, Endomorphism)] -> EndoBimap
fromList ls =
  EndoBimap (IntMap.fromList ls) (Map.fromList $ swap <$> ls)

unsafeLookup :: EndoBimap -> Int -> Endomorphism
unsafeLookup (EndoBimap map' _) i = map' IntMap.! i

safeLookupR :: EndoBimap -> Endomorphism -> Maybe Int
safeLookupR (EndoBimap _ map') e = Map.lookup e map'

data Comp =
  Comp
  { compositions :: Map (Int, Int) Int
  , connected :: IntMap IntSet
  , endomorphisms :: EndoBimap
  , maxKey :: Int
  } deriving (Show, Eq)

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
  max_key <- gets maxKey

  let new_endo = on compose (unsafeLookup _endomorphisms) a b
  let new_endo_key = fromMaybe (succ max_key) $ safeLookupR _endomorphisms new_endo
  let new_endos = insert _endomorphisms new_endo_key new_endo
  let new_comps = Map.insert (a, b) new_endo_key _compositions
  
  let connections = fromMaybe IntSet.empty $ IntMap.lookup b _connected
  let new_connected =
        if new_endo_key `IntMap.member` _connected then
          IntMap.adjust (IntSet.union connections) new_endo_key _connected
        else
          IntMap.insert  new_endo_key connections _connected

  let new_comp =
        comp { compositions = new_comps
             , endomorphisms = new_endos
             , maxKey = new_endo_key
             , connected = new_connected}

  put $
    if (a, b) `Map.member` _compositions then
      comp
    else
      new_comp
  
initEndomorphisms :: (IsState s, Ord k) => DFALexer Int s k -> EndoBimap
initEndomorphisms lexer' = table
  where
    lexer = reenumerateLexer (0 :: Int) lexer'
    _states = states $ fsa lexer
    last_index = Set.size _states - 1
    toArray = Array.array (0, last_index) . zip [0..last_index]
    table =
      fromList
      $ Map.toList
      $ toArray <$> parallelLexingTable lexer

initConnected :: (IsState s, Ord k) => DFALexer Int s k -> IntMap IntSet
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

complete :: (IsState s, Ord t) => DFALexer Int s t -> Comp
complete lexer = execState complete' comp
  where
    max_key = maximum . alphabet $ fsa lexer
    comp =
      Comp
      { connected = initConnected lexer
      , endomorphisms = initEndomorphisms lexer
      , compositions = Map.empty
      , maxKey = max_key}
