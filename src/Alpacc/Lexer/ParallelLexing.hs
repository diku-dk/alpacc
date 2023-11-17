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
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Maybe
import Data.Array (Array, Ix)
import Data.Array qualified as Array hiding (Array)

type Endomorphism = Array Int Int

data Comp =
  Comp
  { compositions :: Map (Endomorphism, Endomorphism) Endomorphism
  , connected :: Map Endomorphism (Set Endomorphism)
  } deriving (Show, Eq)

type CompState t = State Comp t

compose :: Endomorphism -> Endomorphism -> Endomorphism
compose a b =
  Array.array (0, length a - 1)
  $ map (\i -> (i, b Array.! (a Array.! i))) [0..(length a - 1)]

composeMemo :: Endomorphism -> Endomorphism -> CompState ()
composeMemo a b = do
  comp <- get
  _compositions <- gets compositions
  _connected <- gets connected

  let c = fromMaybe (a `compose` b) $ Map.lookup (a, b) _compositions
  let new_compositions = Map.insert (a, b) c _compositions
  let connections = fromMaybe Set.empty $ Map.lookup b _connected
  let new_connected =
        if c `Map.member` _connected then
          Map.adjust (Set.union connections) c _connected
        else
          Map.insert c connections _connected

  put $
    comp { compositions = new_compositions
         , connected = new_connected}

endomorphismTable :: (IsState s, IsTransition t, Ord k) => DFALexer t s k -> Map t Endomorphism
endomorphismTable lexer' = table
  where
    lexer = reenumerateLexer (0 :: Int) lexer'
    _states = states $ fsa lexer
    last_index = Set.size _states - 1
    toArray = Array.array (0, last_index) . zip [0..last_index]
    table = toArray <$> parallelLexingTable lexer

initConnected :: (IsTransition t, IsState s, Ord k) => DFALexer t s k -> Map Endomorphism (Set Endomorphism)
initConnected lexer = Map.fromList $ auxiliary <$> _alphabet
  where
    table = endomorphismTable lexer
    toEndo = (table Map.!)

    dfa = fsa lexer
    _alphabet = Set.toList $ alphabet dfa
    _states = Set.toList $ states dfa
    _transitions = transitions' dfa

    auxiliary t =
      (toEndo t, )
      . Set.map toEndo
      . Set.unions
      $ transitionsLookup
      <$> mapMaybe ((`Map.lookup` _transitions) . (, t)) _states

    transitionLookup s t =
      if (s, t) `Map.member` _transitions
      then Just t
      else Nothing
    
    transitionsLookup s =
      Set.fromList
      $ mapMaybe (transitionLookup s) _alphabet

addCompositions :: Endomorphism -> Set Endomorphism -> CompState ()
addCompositions t t_set = do
  mapM_ (composeMemo t) $ Set.toList t_set

complete' :: CompState ()
complete' = do
  _connected <- gets connected
  old_compositions <- gets compositions
  mapM_ (uncurry addCompositions) $ Map.toList _connected
  new_compositions <- gets compositions
  when (old_compositions /= new_compositions) complete'

complete :: (IsTransition t, IsState s, Ord k) => DFALexer t s k -> Comp
complete lexer = execState complete' comp
  where
    comp =
      Comp
      { connected = initConnected lexer
      , compositions = Map.empty}
