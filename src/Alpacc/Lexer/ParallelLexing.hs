module Alpacc.Lexer.ParallelLexing
  ( ParallelLexer (..)
  , parallelLexer
  , EndoData (..)
  , listCompositions
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
import Data.Array.Base (IArray (..))
import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as UArray hiding (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.Tuple (swap)
import Control.Monad.State.Strict
import Data.List qualified as List

errorMessage :: String
errorMessage = "Error: Happend during Parallel Lexing genration, contact a maintainer."

type S = Int
type E = Int

data Endomorphism =
  Endomorphism
  {-# UNPACK #-} !(UArray S S)
  {-# UNPACK #-} !(UArray S Bool) deriving (Eq, Ord, Show)

data EndoData t =
  EndoData
  { endo :: !E
  , token :: !(Maybe t)
  , isAccepting :: !Bool
  , isProducing :: !Bool
  } deriving (Show, Eq, Ord)

toEndoData ::
  (Enum t, Bounded t, Ord t, Ord k) =>
  ParallelDFALexer t S k ->
  IntMap Endomorphism ->
  E ->
  EndoData k
toEndoData lexer to_endo e =
    EndoData
    { endo = e
    , token = Map.lookup s token_map
    , isAccepting = s `Set.member` accept_states
    , isProducing = is_producing
    }
  where
    initial_state = initial $ fsa $ parDFALexer lexer
    (is_producing, s) =
      case IntMap.lookup e to_endo of 
      Just (Endomorphism endo' producing) | in_bound ->
          (producing UArray.! initial_state
          ,endo' UArray.! initial_state)
        where
          (a, b) = bounds endo'
          in_bound = a <= initial_state && initial_state <= b
      _any -> error errorMessage
    token_map = terminalMap $ parDFALexer lexer
    accept_states = accepting $ fsa $ parDFALexer lexer

data ParallelLexer t e =
  ParallelLexer
  { compositions :: !(IntMap (IntMap e)) 
  , endomorphisms :: !(Map t e)
  , identity :: !e
  , endomorphismsSize :: !Int
  , dead :: !e
  , tokenSize :: !Int
  , acceptArray :: !(UArray E Bool)
  } deriving (Show, Eq, Ord)

data EndoCtx =
  EndoCtx
  { comps ::  !(IntMap (IntMap E))
  , endoMap :: !(Map Endomorphism E)
  , inverseEndoMap :: !(IntMap Endomorphism)
  , connectedMap :: !(IntMap IntSet)
  , inverseConnectedMap :: !(IntMap IntSet)
  , maxE :: !E
  , identityE :: !E
  } deriving (Show, Eq, Ord)

lookupComposition ::
  IntMap (IntMap e) ->
  E ->
  E ->
  Maybe e
lookupComposition comps e e' =
  IntMap.lookup e comps >>= IntMap.lookup e'

listCompositions :: ParallelLexer t e -> [e]
listCompositions parallel_lexer =
  [fromMaybe d $ lookupComposition cs e e' | e <- range, e' <- range]
  where
    range = [0..upper]
    cs = compositions parallel_lexer
    d = dead parallel_lexer
    upper = endomorphismsSize parallel_lexer - 1

endoInsert ::
  E ->
  Endomorphism ->
  State EndoCtx ()
endoInsert e endo = do
  inv_map <- gets inverseEndoMap
  let new_inv_map = IntMap.insert e endo inv_map

  _map <- gets endoMap
  let new_map = Map.insert endo e _map
  
  modify $
    \s ->
      s { inverseEndoMap = new_inv_map
        , endoMap = new_map }

eLookup :: E -> State EndoCtx Endomorphism
eLookup e = do
  inv_map <- gets inverseEndoMap
  pure $ inv_map IntMap.! e

connectedLookup :: E -> State EndoCtx (Maybe IntSet)
connectedLookup e = do
  _map <- gets connectedMap
  pure $ IntMap.lookup e _map

connectedUpdate :: E -> IntSet -> State EndoCtx ()
connectedUpdate e e_set = do
  _map <- gets connectedMap
  inv_map <- gets inverseConnectedMap
  let new_map = IntMap.insertWith IntSet.union e e_set _map
  let new_inverse_map =
        IntMap.unionWith IntSet.union inv_map
        $ IntMap.fromList
        $ (,IntSet.singleton e) <$> IntSet.toList e_set
  modify $ \s ->
    s { connectedMap = new_map
      , inverseConnectedMap = new_inverse_map }

connectedUpdateAll :: IntMap IntSet -> State EndoCtx ()
connectedUpdateAll _map =
  mapM_ (uncurry connectedUpdate) $ IntMap.assocs _map

insertComposition :: E -> E -> E -> State EndoCtx ()
insertComposition e e' e'' = do
  _map <- gets comps
  let new_map =
        if e `IntMap.member` _map then
          IntMap.adjust (IntMap.insert e' e'') e _map
        else
          IntMap.insert e (IntMap.singleton e' e'') _map
  modify $ \s -> s { comps = new_map }

preSets :: E -> E -> State EndoCtx (IntMap IntSet)
preSets e'' e = do
  _map <- gets connectedMap
  inv_map <- gets inverseConnectedMap
  let set = IntSet.singleton e''
  pure $
    case IntMap.lookup e inv_map of
      Nothing -> error errorMessage -- This should never happen.
      Just _set ->
        if e'' `IntSet.member` _set
        then IntMap.empty
        else  
          IntMap.fromList
          $ (,set) <$> IntSet.toList _set

postSets :: E -> E -> State EndoCtx (IntMap IntSet)
postSets e'' e' = do
  _map <- gets connectedMap
  e_set' <- fromMaybe IntSet.empty <$> connectedLookup e'
  e_set'' <- fromMaybe IntSet.empty <$> connectedLookup e''
  pure $ IntMap.singleton e'' (IntSet.difference e_set' e_set'')

endomorphismLookup ::
  Endomorphism ->
  State EndoCtx (Maybe E)
endomorphismLookup endomorphism = do
  _map <- gets endoMap
  pure $ Map.lookup endomorphism _map

compose :: Endomorphism -> Endomorphism -> Endomorphism
compose (Endomorphism a a') (Endomorphism b b') = Endomorphism c c'
  where
    c = UArray.array (0, numElements a - 1)
      $ map auxiliary [0..(numElements a - 1)]
    c' = UArray.array (0, numElements a' - 1)
      $ map auxiliary' [0..(numElements a' - 1)]
    auxiliary i = (i, b UArray.! (a UArray.! i))
    auxiliary' i = (i, b' UArray.! (a UArray.! i))

endoNext :: Endomorphism -> State EndoCtx E
endoNext endo = do
  maybe_e <- endomorphismLookup endo
  case maybe_e of
    Just e -> pure e
    Nothing -> do
      max_e <- gets maxE
      identity <- gets identityE
      insertComposition max_e identity max_e
      insertComposition identity max_e max_e
      let new_max_e = succ max_e
      modify $ \s -> s { maxE = new_max_e }
      pure new_max_e

endoCompose ::
  E ->
  E ->
  State EndoCtx (IntMap IntSet)
endoCompose e e' = do
  _comps <- gets comps
  case lookupComposition _comps e e' of
    Nothing -> do
      endo <- eLookup e
      endo' <- eLookup e'
      let endo'' = endo `compose` endo'
      e'' <- endoNext endo''
      endoInsert e'' endo''
      insertComposition e e' e''
      pre_sets <- preSets e'' e
      post_sets <- postSets e'' e'
      let new_sets = IntMap.unionWith IntSet.union pre_sets post_sets
      connectedUpdateAll new_sets
      pure new_sets
    _ -> pure IntMap.empty

popElement :: IntMap IntSet -> Maybe ((Int, Int), IntMap IntSet)
popElement _map =
  case IntMap.lookupMin _map of
    Just (key, set) ->
      if IntSet.null set
      then popElement (IntMap.delete key _map)
      else
        let e = IntSet.findMin set
            new_map = IntMap.adjust (IntSet.delete e) key _map
         in Just ((key, e), new_map) 
    Nothing -> Nothing

endoCompositionsTable ::
  IntMap IntSet ->
  State EndoCtx ()
endoCompositionsTable _map =
  case popElement _map of
    Just ((e, e'), map') -> do
      map'' <- endoCompose e e'
      let !map''' = IntMap.unionWith IntSet.union map' map''
      endoCompositionsTable map''' 
    Nothing -> pure ()

compositionsTable ::
  (Enum t, Bounded t, Ord t, Ord k) =>
  ParallelDFALexer t S k ->
  (UArray E Bool
  ,Map Endomorphism (EndoData k)
  ,IntMap (IntMap (EndoData k)))
compositionsTable lexer = (accept_array, to_endo, _compositions)
  where
    accept_array =
      toAcceptArray
      $ IntMap.mapWithKey (\k _ -> toEndoData' k) inv_to_endo
    ctx = initEndoCtx lexer
    toEndoData' = toEndoData lexer inv_to_endo
    connected_map = connectedMap ctx
    (EndoCtx
      { comps = _compositions'
      , endoMap = to_endo'
      , inverseEndoMap = inv_to_endo
      }) = execState (endoCompositionsTable connected_map) ctx
    to_endo = fmap toEndoData' to_endo'
    _compositions = fmap toEndoData' <$> _compositions'

endomorphismTable ::
  (Enum t, Bounded t, Ord t, Ord k) =>
  ParallelDFALexer t S k ->
  Map t Endomorphism
endomorphismTable lexer =
  Map.fromList
  $ map statesFromChar
  $ Set.toList _alphabet
  where
    dfa = fsa $ parDFALexer lexer
    produces_set = producesToken lexer
    dead_state = deadState lexer
    _transitions = transitions' dfa
    _states = states dfa
    _alphabet = alphabet dfa
    first_index = minimum _states
    last_index = maximum _states
    tableLookUp key =
      fromMaybe dead_state
      $ Map.lookup key _transitions
    statesFromChar t = (t, Endomorphism ss bs)
      where
        ss =
          UArray.array (first_index, last_index)
          $ zip [first_index..last_index]
          $ map (tableLookUp . (, t))
          $ Set.toAscList _states
        bs =
          UArray.array (first_index, last_index)
          $ zip [first_index..last_index]
          $ map ((`Set.member` produces_set) . (, t))
          $ Set.toAscList _states

connectedTable :: Ord t => ParallelDFALexer t S k -> Map t (Set t)
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

toAcceptArray :: IntMap (EndoData k) -> UArray E Bool
toAcceptArray endo_map =
  UArray.array (fst $ IntMap.findMin endo_map
               ,fst $ IntMap.findMax endo_map)
  $ IntMap.assocs
  $ isAccepting <$> endo_map

initCompositions :: E -> IntSet -> IntMap (IntMap E)
initCompositions identity set =
  IntMap.unionsWith IntMap.union
  $ [IntMap.singleton identity (IntMap.singleton e e) | e <- ls]
  ++ [IntMap.singleton e (IntMap.singleton identity e) | e <- ls]
  where
    ls = IntSet.toList set

initEndoCtx ::
  (Enum t, Bounded t, Ord t, Ord k) =>
  ParallelDFALexer t S k ->
  EndoCtx
initEndoCtx lexer =
  EndoCtx
  { comps = initCompositions 0 $ IntMap.keysSet e_to_endo
  , endoMap = endo_to_e
  , inverseEndoMap = e_to_endo
  , connectedMap = connected_table
  , inverseConnectedMap = inverse_connected_table
  , maxE = maximum endo_to_e
  , identityE = 0
  }
  where
    endo_table = endomorphismTable lexer
    e_to_endo =
      IntMap.fromList
      $ zip [0 :: E ..]
      $ List.nub
      $ (identityEndomorphism lexer :)
      $ (deadEndomorphism lexer :)
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
    toMap k =
      IntMap.fromList
      . fmap (,IntSet.singleton k)
      . IntSet.toList
    inverse_connected_table =
      IntMap.unionsWith IntSet.union
      $ IntMap.mapWithKey toMap connected_table

deadEndomorphism ::
  (Ord t, Enum t, Bounded t, Ord k) =>
  ParallelDFALexer t S k ->
  Endomorphism
deadEndomorphism lexer = Endomorphism s b
  where
    _states = states $ fsa $ parDFALexer lexer
    first_state = minimum _states
    last_state = maximum _states
    dead_state = deadState lexer
    s =
      UArray.array (first_state, last_state)
      $ (,dead_state) <$> [first_state..last_state]
    b =
      UArray.array (first_state, last_state)
      $ (,False) <$> [first_state..last_state]

identityEndomorphism ::
  (Ord t, Enum t, Bounded t, Ord k) =>
  ParallelDFALexer t S k ->
  Endomorphism
identityEndomorphism lexer = Endomorphism s b
  where
    _states = states $ fsa $ parDFALexer lexer
    first_state = minimum _states
    last_state = maximum _states
    s =
      UArray.array (first_state, last_state)
      $ zip [first_state..last_state] [first_state..last_state]
    b =
      UArray.array (first_state, last_state)
      $ (,False) <$> [first_state..last_state]

parallelLexer ::
  (Ord t, Enum t, Bounded t, Ord s, Ord k) =>
  ParallelDFALexer t s k ->
  ParallelLexer t (EndoData k)
parallelLexer lexer' =
  ParallelLexer
  { compositions = _compositions
  , endomorphisms = _transitions_to_endo
  , identity = identity_e
  , endomorphismsSize = endo_size
  , dead = dead_e
  , tokenSize = Map.size $ terminalMap $ parDFALexer lexer
  , acceptArray = accept_array
  }
  where
    lexer = enumerateParLexer 0 lexer'
    (accept_array, to_endo, _compositions) = compositionsTable lexer
    endo_size = Map.size to_endo
    toEndo x = to_endo Map.! x
    dead_e = toEndo $ deadEndomorphism lexer
    identity_e = toEndo $ identityEndomorphism lexer
    _unknown_transitions =
      Map.fromList
      $ map (,dead_e) [minBound..maxBound]
    _transitions_to_endo =
      (`Map.union` _unknown_transitions)
      $ toEndo <$> endomorphismTable lexer
  
