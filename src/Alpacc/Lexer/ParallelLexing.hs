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
  deriving (Show, Eq)

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

data Comp t =
  Comp
  { compositions :: Map (Int, Int) Int
  , connected :: IntMap IntSet
  , endomorphisms :: EndoMap
  , maxKeyComp :: Int
  } deriving (Show, Eq)

type CompState t a = State (Comp t) a

compose :: Endomorphism -> Endomorphism -> Endomorphism
compose a b =
  Array.array (0, length a - 1)
  $ map (\i -> (i, b Array.! (a Array.! i))) [0..(length a - 1)]

compose' :: (Show t, Ord t) => Int -> Int -> CompState t ()
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

invertMap :: (Ord a, Ord b) => Map a b -> Map b (Set a) 
invertMap =
  Map.unionsWith Set.union
  . fmap (uncurry Map.singleton . second Set.singleton . swap)
  . Map.assocs

minimumTerminal :: (Foldable t, Ord a) => t a -> a
minimumTerminal a = if null a then error "The minimum terminal could not be found." else minimum a

tokenMap :: (Ord k, Show k, IsTransition t, IsState s) => DFALexer t s k -> Map ((s, s), t) k
tokenMap lexer = minimumTerminal <$> fixedPointIterate (/=) next terminal_map
  where
    dfa = fsa lexer
    initial_state = initial dfa
    inverted_transitions = invertMap $ transitions' dfa
    terminal_map = Map.mapKeys (second runIdentity) $ terminalMap lexer

    next term_map =
      Map.mapWithKey (auxiliary inverted_transitions term_map) term_map
    
    auxiliary inv_trans term_map ((s, _), _) set =
      if initial_state == s then
        set
      else
        temp `Set.intersection` set
      where
        temp =
          Set.unions
          $ Set.map (\(s', t) ->
                       fromMaybe Set.empty
                       $ Map.lookup ((s', s), t) term_map)
          $ fromMaybe Set.empty
          $ Map.lookup s inv_trans

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

addCompositions :: (Show t, Ord t) => Int -> IntSet -> CompState t ()
addCompositions t t_set = do
  mapM_ (compose' t) $ IntSet.toList t_set

complete' :: (Show t, Ord t) => CompState t ()
complete' = do
  _connected <- gets connected
  old_compositions <- gets compositions
  mapM_ (uncurry addCompositions) $ IntMap.toList _connected
  new_compositions <- gets compositions
  when (old_compositions /= new_compositions) complete'

stateMap :: Int -> EndoMap -> IntMap Int
stateMap init' (EndoMap map' _) = (Array.! init') <$> map'

complete :: (Ord t, Show t) => IntLexer t -> (IntMap Int, Map (Int, Int) Int, Map ((Int, Int), Int) t, Int, Int, Int)
complete lexer =
  ( stateMap initial_state $ endomorphisms final_comp 
  , compositions final_comp
  , tokenMap lexer
  , dead_state
  , succ $ maxKeyComp final_comp
  , succ $ succ $ maxKeyComp final_comp)
  where
    final_comp = execState complete' comp
    max_key = maximum . alphabet $ fsa lexer
    initial_state = initial $ fsa lexer
    dead_state = maximum . states $ fsa lexer
    _endomorphisms = initEndomorphisms dead_state lexer
    comp =
      Comp
      { connected = initConnected lexer
      , endomorphisms = _endomorphisms
      , compositions = Map.empty
      , maxKeyComp = max_key}
