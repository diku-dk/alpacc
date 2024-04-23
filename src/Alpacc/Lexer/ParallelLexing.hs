module Alpacc.Lexer.ParallelLexing
  ( intParallelLexer
  , ParallelLexer (..)
  , IntParallelLexer (..)
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
import Data.Either.Extra
import Data.Function
import Data.Bits

errorMessage :: String
errorMessage = "Error: Happend during Parallel Lexing genration, contact a maintainer."

type S = Int
type E = Int
type Endomorphism = UArray S S

data EndoData t =
  EndoData
  { endo :: E
  , token :: Maybe t
  , isAccepting :: Bool
  , isIgnore :: Bool
  } deriving (Show, Eq, Ord)

maskSizes ::
  Int ->
  Int ->
  Either String (Int, Int, Int, Int)
maskSizes endo_size token_size =
  if sum_size <= 64
  then Left "Error: The parser cannot be create due to too mamy tokens or compositions."
  else Right (endo_mask_size
             ,token_mask_size
             ,accept_mask_size
             ,ignore_mask_size)
  where
    int_size = finiteBitSize (zeroBits :: Int)
    endo_clz = countLeadingZeros (max 1 (endo_size - 1))
    token_clz = countLeadingZeros (max 1 (token_size - 1))
    endo_mask_size = int_size - endo_clz
    token_mask_size = int_size - token_clz
    accept_mask_size = 1
    ignore_mask_size = 1
    sum_size =
      endo_mask_size +
      token_mask_size +
      accept_mask_size +
      ignore_mask_size

masks :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) 
masks (endo_mask_size
      ,token_mask_size
      ,accept_mask_size
      ,ignore_mask_size) = (endo_mask
                           ,token_mask
                           ,accept_mask
                           ,ignore_mask)
  where
    off0 = endo_mask_size
    off1 = off0 + token_mask_size
    off2 = off1 + accept_mask_size
    endo_mask = shift 1 endo_mask_size - 1
    token_mask = shift (shift 1 token_mask_size - 1) off0
    accept_mask = shift (shift 1 accept_mask_size - 1) off1
    ignore_mask = shift (shift 1 ignore_mask_size - 1) off2

endoDataToInt ::
  Ord k =>
  (Int, Int, Int, Int) ->
  Map (Maybe k) Int ->
  EndoData k ->
  Either String Int
endoDataToInt (endo_mask_size
              ,token_mask_size
              ,accept_mask_size
              ,_) to_int endo_data = do
  t_int <- findInt maybe_token
  return $
    e + shift t_int off0 + shift a_int off1 + shift i_int off2
  where
    off0 = endo_mask_size
    off1 = off0 + token_mask_size
    off2 = off1 + accept_mask_size
    EndoData { endo = e
             , token = maybe_token
             , isAccepting = accept 
             , isIgnore = ignore
             } = endo_data
    a_int = fromEnum accept
    i_int = fromEnum ignore
    findInt = maybeToEither errorMessage . flip Map.lookup to_int

toEndoData ::
  (Enum t, Bounded t, IsTransition t, Ord k) =>
  k ->
  ParallelDFALexer t S k ->
  IntMap Endomorphism ->
  E ->
  Either String (EndoData k)
toEndoData is_ignore lexer to_endo e = do
  s <- maybeToEither errorMessage $ toState e
  let maybe_token = toToken s
  return $
    EndoData
    { endo = e
    , token = maybe_token
    , isAccepting = s `Set.member` accept_states
    , isIgnore = Just is_ignore == maybe_token
    }
  where
    initial_state = initial $ fsa $ parDFALexer lexer
    toState e' = do
      endo' <- IntMap.lookup e' to_endo
      let (a, b) = bounds endo'
      if a <= initial_state && initial_state <= b
        then return $ endo' UArray.! initial_state
        else Nothing
    token_map = terminalMap $ parDFALexer lexer
    toToken s = Map.lookup s token_map
    accept_states = accepting $ fsa $ parDFALexer lexer

createProducesTokenSet ::
  (IsTransition t, Enum t, Bounded t, Ord k) =>
  k ->
  ParallelDFALexer t S k ->
  IntMap Endomorphism ->
  Either String (Set (EndoData k, t))
createProducesTokenSet is_ignore lexer to_endo = do
  pairs <- sequence $ mapMaybe toTuples $ IntMap.keys to_endo

  return $ Set.fromList $ concat pairs
  where
    toEndoData' = toEndoData is_ignore lexer to_endo
    initial_state = initial $ fsa $ parDFALexer lexer
    produces_token =
      IntMap.fromList
      $ mapMaybe toList
      $ List.groupBy (on (==) fst)
      $ List.sortOn fst
      $ Set.toList
      $ producesToken lexer
    toList ((e, t):xs) = Just (e, t : map snd xs)
    toList _ = Nothing
    toState e = (to_endo IntMap.! e) UArray.! initial_state
    toTuples e = do
      ts <- IntMap.lookup (toState e) produces_token
      let x = do
            y <- toEndoData' e
            return $ (y,) <$> ts
      return x

data ParallelLexer t e =
  ParallelLexer
  { compositions :: Map (E, E) e 
  , endomorphisms :: Map t e
  , producesTokenSet :: Set (e, t) 
  , identity :: e
  , endomorphismsSize :: Int
  } deriving (Show, Eq, Ord)

data IntParallelLexer t =
  IntParallelLexer
  { parLexer :: ParallelLexer t Int
  , endoMask :: Int
  , tokenMask :: Int
  , isAcceptingMask :: Int
  , isIgnoreMask :: Int
  } deriving (Show, Eq, Ord)

intParallelLexer ::
  (IsTransition t, Enum t, Bounded t, Ord k) =>
  k ->
  Map (Maybe k) Int ->
  ParallelDFALexer t S k ->
  Either String (IntParallelLexer t)
intParallelLexer is_ignore to_int lexer = do
  (endo_size, parallel_lexer) <- parallelLexer is_ignore lexer
  let token_size = Map.size to_int
  size_tuple <- maskSizes endo_size token_size
  let _masks = masks size_tuple
  let endoDataToInt' = endoDataToInt size_tuple to_int
  let (endo_mask, token_mask,
       accept_mask, ignore_mask) = masks size_tuple
  new_compositions <-
    mapM endoDataToInt' (compositions parallel_lexer)
  new_endomorphims <-
    mapM endoDataToInt' (endomorphisms parallel_lexer)
  new_produces_token <-
    fmap Set.fromList
    $ mapM (\(a, b) -> do
               x <- endoDataToInt' a
               return (x, b))
    $ Set.toList
    $ producesTokenSet parallel_lexer
  new_identity <-
    endoDataToInt' $ identity parallel_lexer
  let new_parallel_lexer =
        parallelLexer
        { compositions = new_compositions
        , endomorphisms = new_endomorphims
        , producesTokenSet = new_produces_token
        , identity = new_identity
        }
  return $
    IntParallelLexer
    { parLexer = new_parallel_lexer
    , endoMask = endo_mask
    , tokenMask = token_mask
    , isAcceptingMask = accept_mask
    , isIgnoreMask = ignore_mask
    }
  
data EndoCtx =
  EndoCtx
  { comps :: Map (E, E) E
  , endoMap :: Map Endomorphism E
  , inverseEndoMap :: IntMap Endomorphism
  , connectedMap :: IntMap IntSet
  , inverseConnectedMap :: IntMap IntSet
  , maxE :: E
  } deriving (Show, Eq, Ord)

type EndoState = State EndoCtx

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
  inv_map <- gets inverseConnectedMap
  let new_map = IntMap.insertWith IntSet.union e e_set _map
  let new_inverse_map =
        IntMap.unionWith IntSet.union inv_map
        $ IntMap.fromList
        $ (,IntSet.singleton e) <$> IntSet.toList e_set
  modify $ \s ->
    s { connectedMap = new_map
      , inverseConnectedMap = new_inverse_map }

connectedUpdateAll :: IntMap IntSet -> EndoState ()
connectedUpdateAll _map =
  mapM_ (uncurry connectedUpdate) $ IntMap.assocs _map

insertComposition :: E -> E -> E -> EndoState ()
insertComposition e e' e'' = do
  _map <- gets comps
  let new_map = Map.insert (e, e') e'' _map
  modify $ \s -> s { comps = new_map }

preSets :: E -> E -> EndoState (IntMap IntSet)
preSets e'' e = do
  _map <- gets connectedMap
  inv_map <- gets inverseConnectedMap
  let set = IntSet.singleton e''
  return $
    case IntMap.lookup e inv_map of
      Nothing -> error errorMessage -- This should never happen.
      Just _set ->
        if e'' `IntSet.member` _set
        then IntMap.empty
        else  
          IntMap.fromList
          $ (,set) <$> IntSet.toList _set

postSets :: E -> E -> EndoState (IntMap IntSet)
postSets e'' e' = do
  _map <- gets connectedMap
  e_set' <- fromMaybe IntSet.empty <$> connectedLookup e'
  e_set'' <- fromMaybe IntSet.empty <$> connectedLookup e''
  return $ IntMap.singleton e'' (IntSet.difference e_set' e_set'')

endomorphismLookup :: Endomorphism -> EndoState (Maybe E)
endomorphismLookup endomorphism = do
  _map <- gets endoMap
  return $ Map.lookup endomorphism _map

compose :: Endomorphism -> Endomorphism -> Endomorphism
compose a b =
  UArray.array (0, numElements a - 1)
  $ map auxiliary [0..(numElements a - 1)]
  where
    auxiliary i = (i, b UArray.! (a UArray.! i))

endoNext :: EndoState E
endoNext = do
  max_e <- gets maxE
  let new_max_e = succ max_e
  modify $ \s -> s { maxE = new_max_e }
  return new_max_e

endoCompose :: E -> E -> EndoState (IntMap IntSet)
endoCompose e e' = do
  maybe_endo <- eLookup e
  maybe_endo' <- eLookup e'
  case (maybe_endo, maybe_endo') of
    (Just endo, Just endo') -> do
      _comps <- gets comps
      case Map.lookup (e, e') _comps of
        Just _ -> return IntMap.empty
        Nothing -> do
          let endo'' = endo `compose` endo'
          maybe_e'' <- endomorphismLookup endo''
          e'' <- maybe endoNext return maybe_e''
          endoInsert e'' endo''
          insertComposition e e' e''
          pre_sets <- preSets e'' e
          post_sets <- postSets e'' e'
          let new_sets =
                IntMap.unionWith IntSet.union pre_sets post_sets
          connectedUpdateAll new_sets
          return new_sets
    _ -> error errorMessage -- This should never happen.

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

endoCompositionsTable :: IntMap IntSet -> EndoState ()
endoCompositionsTable _map =
  case popElement _map of
    Just ((e, e'), map') -> do
      map'' <- endoCompose e e'
      let !map''' = IntMap.unionWith IntSet.union map' map''
      endoCompositionsTable map''' 
    Nothing -> return ()

compositionsTable ::
  (Enum t, Bounded t, IsTransition t, Ord k) =>
  k ->
  ParallelDFALexer t S k ->
  Either String (Map Endomorphism (EndoData k)
                ,Set (EndoData k, t)
                ,Map (E, E) (EndoData k))
compositionsTable is_ignore lexer = do
  a <- to_endo
  b <- produces_token
  c <- _compositions
  return (a, b, c)
  where
    ctx = initEndoCtx lexer
    toEndoData' = toEndoData is_ignore lexer inv_to_endo
    connected_map = connectedMap ctx
    (EndoCtx
      { comps = _compositions'
      , endoMap = to_endo'
      , inverseEndoMap = inv_to_endo'
      }) = execState (endoCompositionsTable connected_map) ctx
    vec_dead = deadEndomorphism lexer
    vec_identity = identityEndomorphism lexer
    produces_token =
      createProducesTokenSet is_ignore lexer inv_to_endo
    to_endo =
      mapM toEndoData'
      $ Map.insert vec_dead _dead
      $ Map.insert vec_identity _identity to_endo'
    inv_to_endo =
      IntMap.insert _dead vec_dead
      $ IntMap.insert _identity vec_identity inv_to_endo'
    _identity =
      case Map.lookup vec_identity to_endo' of
        Nothing -> succ $ maximum to_endo'
        Just a -> a
    _dead =
      case Map.lookup vec_dead to_endo' of
        Nothing -> succ _identity
        Just a -> a
    _compositions =
      mapM toEndoData'
      $ addDead _dead
      $ addIdentity _identity _compositions'

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
      UArray.array (first_index, last_index)
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
  , inverseConnectedMap = inverse_connected_table
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
    toMap k =
      IntMap.fromList
      . fmap (,IntSet.singleton k)
      . IntSet.toList
    inverse_connected_table =
      IntMap.unionsWith IntSet.union
      $ IntMap.mapWithKey toMap connected_table

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
  UArray.array (first_state, last_state)
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
  UArray.array (first_state, last_state)
  $ zip [first_state..last_state] [first_state..last_state]
  where
    _states = states $ fsa $ parDFALexer lexer
    first_state = minimum _states
    last_state = maximum _states
    
parallelLexer ::
  (IsTransition t, Enum t, Bounded t, Ord k) =>
  k ->
  ParallelDFALexer t S k ->
  Either String (Int, ParallelLexer t (EndoData k))
parallelLexer is_ignore lexer = do
  (to_endo, produces_token, _compositions) <- compositionsTable is_ignore lexer
  let endo_size = Map.size to_endo
  let toEndo x = maybeToEither errorMessage $ Map.lookup x to_endo
  dead_e <- toEndo $ deadEndomorphism lexer
  identity_e <- toEndo $ identityEndomorphism lexer
  let _dead_transitions =
        Map.fromList
        $ map (,dead_e) [minBound..maxBound]
  _transitions_to_endo <-
        fmap (`Map.union` _dead_transitions)
        $ mapM toEndo
        $ endomorphismTable lexer
  return
    (endo_size
    ,ParallelLexer
     { compositions = _compositions
     , producesTokenSet = produces_token
     , endomorphisms = _transitions_to_endo
     , identity = identity_e
     , endomorphismsSize = endo_size
     })
