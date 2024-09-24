module Alpacc.Lexer.ParallelLexing
  ( intParallelLexer
  , ParallelLexer (..)
  , IntParallelLexer (..)
  )
where

import Alpacc.Types
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
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty hiding (NonEmpty)
import Data.Either.Extra
import Data.Bits
import Data.Word
import Control.Monad

errorMessage :: String
errorMessage = "Error: Happend during Parallel Lexing genration, contact a maintainer."

type S = Int
type E = Int

-- | An extended endomorphism
data ExtEndo =
  ExtEndo !(UArray S S) !(UArray S Bool) deriving (Eq, Ord, Show)

data ExtEndoData t =
  ExtEndoData
  { endo :: !E
  , token :: !(Maybe t)
  , isAccepting :: !Bool
  , isProducing :: !Bool
  } deriving (Show, Eq, Ord)

data Mask64 =
  Mask64
  { mask :: !Int
  , offset :: !Int
  } deriving (Show, Eq, Ord)

newtype Masks64 = Masks64 (NonEmpty Mask64)

findSize :: Int -> Int
findSize = (int_size-) . countLeadingZeros . max 1 . (-1)
  where
    int_size = finiteBitSize (zeroBits :: Int)

masks :: [Int] -> Either String Masks64
masks sizes = do
  unless (any (0<) sizes) $ Left "Error: Negative sizes were used to encode the masks for the states in a data parallel lexer. This should not happen, contact a maintainer."
  unless (sum bit_sizes > 64) $ Left "Error: There are too many tokens and/or states to create a data parallel lexer."
  let offsets = init $ List.scanl' (+) 0 bit_sizes -- Exclusive scan.
  let _masks = zipWith shift offsets bit_sizes
  pure $ Masks64 $ NonEmpty.fromList $ zipWith Mask64 _masks offsets
  where
    bit_sizes = findSize <$> sizes

data ParallelLexerMasks =
  ParallelLexerMasks
  { tokenMask :: !Int
  , tokenOffset :: !Int
  , indexMask :: !Int
  , indexOffset :: !Int
  , producingMask :: !Int
  , producingOffset :: !Int
  } deriving (Eq, Ord, Show)

extEndoType :: ParallelLexer t k -> Either String UInt
extEndoType (ParallelLexer { endomorphismsSize = a, tokenSize = b }) =
  toIntType $ (2^) . findSize <$> [a, b, 1]

lexerMasks :: ParallelLexer t k -> Either String ParallelLexerMasks
lexerMasks (ParallelLexer { endomorphismsSize = e, tokenSize = t }) = do
  Masks64 ls <- masks [e, t, 1]
  let [(index_mask, index_off),
       (toke_mask, token_off),
       (produce_mask, produce_off)] = toList ls
  pure $
    ParallelLexerMasks
    { tokenMask = token_mask
    , tokenOffset = token_off
    , indexMask = index_mask
    , indexOffset = index_off
    , producingMask = produce_mask
    , producingOffset = produce_off
    }
  
newtype ExtEndoEncoded = ExtEndoEncoded Int

endoDataToInt ::
  Ord k =>
  (Int, Int, Int) ->
  Map (Maybe k) Int ->
  ExtEndoData k ->
  Either String Int
endoDataToInt (endo_mask_size
              ,token_mask_size
              ,_) to_int endo_data = do
  t_int <- findInt maybe_token
  return $
    e +
    shift t_int token_off +
    shift p_int produce_off
  where
    token_off = endo_mask_size
    produce_off = token_off + token_mask_size
    ExtEndoData { endo = e
             , token = maybe_token
             , isProducing = produce
             } = endo_data
    p_int = fromEnum produce
    findInt = maybeToEither errorMessage . flip Map.lookup to_int

toExtEndoData ::
  (Enum t, Bounded t, IsTransition t, Ord k) =>
  ParallelDFALexer t S k ->
  IntMap ExtEndo ->
  E ->
  Either String (ExtEndoData k)
toExtEndoData lexer to_endo e = do
  (is_producing, s) <- maybeToEither errorMessage $ toState e
  let maybe_token = toToken s
  return $
    ExtEndoData
    { endo = e
    , token = maybe_token
    , isAccepting = s `Set.member` accept_states
    , isProducing = is_producing
    }
  where
    initial_state = initial $ fsa $ parDFALexer lexer
    toState e' = do
      ExtEndo endo' producing <- IntMap.lookup e' to_endo
      let (a, b) = bounds endo'
      if a <= initial_state && initial_state <= b
        then return (producing UArray.! initial_state
                    ,endo' UArray.! initial_state)
        else Nothing
    token_map = terminalMap $ parDFALexer lexer
    toToken s = Map.lookup s token_map
    accept_states = accepting $ fsa $ parDFALexer lexer
      
data ParallelLexer t e =
  ParallelLexer
  { compositions :: Map (E, E) e 
  , endomorphisms :: Map t e 
  , identity :: e
  , tokenSize :: Int
  , endomorphismsSize :: Int
  , acceptArray :: UArray E Bool 
  } deriving (Show, Eq, Ord)

data IntParallelLexer t =
  IntParallelLexer
  { parLexer :: ParallelLexer t Int
  , parMasks :: ParallelLexerMasks
  } deriving (Show, Eq, Ord)

intParallelLexer ::
  (IsTransition t, Enum t, Bounded t, Ord k, Show k) =>
  Map (Maybe k) Int ->
  ParallelDFALexer t S k ->
  Either String (IntParallelLexer t)
intParallelLexer to_int lexer = do
  (endo_size, parallel_lexer) <- parallelLexer lexer
  let token_size = Map.size to_int
  size_tuple@(a, b, c) <- maskSizes endo_size token_size
  let _masks = masks size_tuple
  let endoDataToInt' = endoDataToInt size_tuple to_int
  let (endo_mask, token_mask, produce_mask) = masks size_tuple
  new_compositions <-
    mapM endoDataToInt' (compositions parallel_lexer)
  new_endomorphims <-
    mapM endoDataToInt' (endomorphisms parallel_lexer)
  new_identity <-
    endoDataToInt' $ identity parallel_lexer
  let new_parallel_lexer =
        parallel_lexer
        { compositions = new_compositions
        , endomorphisms = new_endomorphims
        , identity = new_identity
        }
  return $
    IntParallelLexer
    { parLexer = new_parallel_lexer
    , parMasks = undefined
    }
  
data ExtEndoCtx k =
  ExtEndoCtx
  { comps :: Map (E, E) E
  , endoMap :: Map ExtEndo E
  , inverseEndoMap :: IntMap ExtEndo
  , endoData :: IntMap (ExtEndoData t)
  , initialStateCtx :: S
  , deadStateCtx :: S
  , connectedMap :: IntMap IntSet
  , inverseConnectedMap :: IntMap IntSet
  , maxE :: E
  } deriving (Show, Eq, Ord)

endoInsert ::
  E ->
  ExtEndo ->
  State (ExtEndoCtx k) ()
endoInsert e endo = do
  inv_map <- gets inverseEndoMap
  let new_inv_map = IntMap.insert e endo inv_map

  _map <- gets endoMap
  let new_map = Map.insert endo e _map
  
  modify $
    \s ->
      s { inverseEndoMap = new_inv_map
        , endoMap = new_map }

eLookup :: E -> State (ExtEndoCtx k) (Maybe ExtEndo)
eLookup e = do
  inv_map <- gets inverseEndoMap
  return $ IntMap.lookup e inv_map

connectedLookup :: E -> State (ExtEndoCtx k) (Maybe IntSet)
connectedLookup e = do
  _map <- gets connectedMap
  return $ IntMap.lookup e _map

connectedUpdate :: E -> IntSet -> State (ExtEndoCtx k) ()
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

connectedUpdateAll :: IntMap IntSet -> State (ExtEndoCtx k) ()
connectedUpdateAll _map =
  mapM_ (uncurry connectedUpdate) $ IntMap.assocs _map

insertComposition :: E -> E -> E -> State (ExtEndoCtx k) ()
insertComposition e e' e'' = do
  _map <- gets comps
  let new_map = Map.insert (e, e') e'' _map
  modify $ \s -> s { comps = new_map }

preSets :: E -> E -> State (ExtEndoCtx k) (IntMap IntSet)
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

postSets :: E -> E -> State (ExtEndoCtx k) (IntMap IntSet)
postSets e'' e' = do
  _map <- gets connectedMap
  e_set' <- fromMaybe IntSet.empty <$> connectedLookup e'
  e_set'' <- fromMaybe IntSet.empty <$> connectedLookup e''
  return $ IntMap.singleton e'' (IntSet.difference e_set' e_set'')

endomorphismLookup ::
  ExtEndo ->
  State (ExtEndoCtx k) (Maybe E)
endomorphismLookup endomorphism = do
  _map <- gets endoMap
  return $ Map.lookup endomorphism _map

compose :: ExtEndo -> ExtEndo -> ExtEndo
compose (ExtEndo a a') (ExtEndo b b') = ExtEndo c c'
  where
    c = UArray.array (0, numElements a - 1)
      $ map auxiliary [0..(numElements a - 1)]
    c' = UArray.array (0, numElements a' - 1)
      $ map auxiliary' [0..(numElements a' - 1)]
    auxiliary i = (i, b UArray.! (a UArray.! i))
    auxiliary' i = (i, b' UArray.! (a UArray.! i))

endoNext :: State (ExtEndoCtx k) E
endoNext = do
  max_e <- gets maxE
  let new_max_e = succ max_e
  modify $ \s -> s { maxE = new_max_e }
  return new_max_e

endoCompose ::
  E ->
  E ->
  State (ExtEndoCtx k) (IntMap IntSet)
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

endoCompositionsTable ::
  IntMap IntSet ->
  State (ExtEndoCtx k) ()
endoCompositionsTable _map =
  case popElement _map of
    Just ((e, e'), map') -> do
      map'' <- endoCompose e e'
      let !map''' = IntMap.unionWith IntSet.union map' map''
      endoCompositionsTable map''' 
    Nothing -> return ()

compositionsTable ::
  (Enum t, Bounded t, IsTransition t, Ord k) =>
  ParallelDFALexer t S k ->
  Either String (Map ExtEndo (ExtEndoData k)
                ,Map (E, E) (ExtEndoData k))
compositionsTable lexer = do
  a <- to_endo
  b <- _compositions
  return (a, b)
  where
    ctx = initExtEndoCtx lexer
    toExtEndoData' = toExtEndoData lexer inv_to_endo
    connected_map = connectedMap ctx
    (ExtEndoCtx
      { comps = _compositions'
      , endoMap = to_endo'
      , inverseEndoMap = inv_to_endo'
      }) = execState (endoCompositionsTable connected_map) ctx
    vec_dead = deadExtEndo lexer
    vec_identity = identityExtEndo lexer
    to_endo =
      mapM toExtEndoData'
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
      mapM toExtEndoData'
      $ addDead _dead
      $ addIdentity _identity _compositions'

endomorphismTable ::
  (Enum t, Bounded t, IsTransition t, Ord k) =>
  ParallelDFALexer t S k ->
  Map t ExtEndo
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
    statesFromChar t = (t, ExtEndo ss bs)
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

initExtEndoCtx ::
  (Enum t, Bounded t, IsTransition t, Ord k) =>
  ParallelDFALexer t S k ->
  ExtEndoCtx k
initExtEndoCtx lexer =
  ExtEndoCtx
  { comps = Map.empty
  , endoMap = endo_to_e
  , inverseEndoMap = e_to_endo
  , connectedMap = connected_table
  , inverseConnectedMap = inverse_connected_table
  , maxE = maximum endo_to_e
  , initialStateCtx = initial $ fsa $ parDFALexer lexer
  , deadStateCtx = deadState lexer
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

deadExtEndo ::
  (IsTransition t, Enum t, Bounded t, Ord k) =>
  ParallelDFALexer t S k ->
  ExtEndo
deadExtEndo lexer = ExtEndo s b
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

identityExtEndo ::
  (IsTransition t, Enum t, Bounded t, Ord k) =>
  ParallelDFALexer t S k ->
  ExtEndo
identityExtEndo lexer = ExtEndo s b
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
  (IsTransition t, Enum t, Bounded t, Ord k, Show k) =>
  ParallelDFALexer t S k ->
  Either String (Int, ParallelLexer t (ExtEndoData k))
parallelLexer lexer = do
  (to_endo, _compositions) <- compositionsTable lexer
  let endo_size = Map.size to_endo
  let toEndo x = maybeToEither errorMessage $ Map.lookup x to_endo
  dead_e <- toEndo $ deadExtEndo lexer
  identity_e <- toEndo $ identityExtEndo lexer
  let _unknown_transitions =
        Map.fromList
        $ map (,dead_e) [minBound..maxBound]
  _transitions_to_endo <-
        fmap (`Map.union` _unknown_transitions)
        $ mapM toEndo
        $ endomorphismTable lexer
  return
    (endo_size
    ,ParallelLexer
     { compositions = _compositions
     , endomorphisms = _transitions_to_endo
     , identity = identity_e
     , endomorphismsSize = endo_size
     })
