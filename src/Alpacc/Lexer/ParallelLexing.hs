module Alpacc.Lexer.ParallelLexing
  ( intParallelLexer
  , ParallelLexer (..)
  , IntParallelLexer (..)
  )
where

import Alpacc.Types
import Alpacc.Lexer.FSA
import Alpacc.Lexer.DFA
import Data.Function
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map hiding (Map)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap hiding (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet hiding (IntSet)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Maybe
import Data.Array.Base (IArray (..), UArray (..))
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
import Control.Monad
import Control.Monad.Except (throwError)
import Data.Foldable (Foldable (..))

errorMessage :: String
errorMessage = "Error: Happend during Parallel Lexing genration, contact a maintainer."

type S = Int
type E = Int

deadState :: S
deadState = 0

initState :: S
initState = 1

deadEndo :: E
deadEndo = 0

deadEndoData :: ExtEndoData k
deadEndoData =
  ExtEndoData
  { endo = deadEndo
  , token = Nothing
  , isAccepting = False
  , isProducing = False
  }

identityEndo :: E
identityEndo = 1

identityEndoData :: ExtEndoData k
identityEndoData =
  ExtEndoData
  { endo = identityEndo
  , token = Nothing
  , isAccepting = False
  , isProducing = False
  }

initEndo :: E
initEndo = 2

-- | An extended endomorphism
data ExtEndo =
  ExtEndo !(UArray S S) !(UArray S Bool) deriving (Eq, Ord, Show)

type EndoState t s k a = StateT (EndoCtx t s k) (Either String) a

data ExtEndoData k =
  ExtEndoData
  { endo :: !E
  , token :: !(Maybe k)
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
findSize = (int_size-) . countLeadingZeros . max 1 . pred
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

      
data ParallelLexer t e =
  ParallelLexer
  { compositions :: Map (E, E) e 
  , endomorphisms :: Map t e
  , identity :: e
  , tokenSize :: Int
  , endomorphismsSize :: Int
  , acceptArray :: UArray E Bool 
  } deriving (Show, Eq, Ord)


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
  maybeToEither errorMessage . toIntType . (2^)  . sum $ findSize <$> [a, b, 1]

parallelLexerToMasks64 :: ParallelLexer t k -> Either String Masks64
parallelLexerToMasks64 (ParallelLexer { endomorphismsSize = e, tokenSize = t }) =
  masks [e, t, 1]

encodeMasks64 :: Masks64 -> NonEmpty Int -> Either String Int
encodeMasks64 (Masks64 ms) elems
  | NonEmpty.length ms == NonEmpty.length elems =
    let x = offset <$> ms
    in pure $ sum $ NonEmpty.zipWith (flip shift) elems x 
  | otherwise = Left errorMessage

masks64ToParallelLexerMasks :: Masks64 -> Either String ParallelLexerMasks 
masks64ToParallelLexerMasks (Masks64 ls) = do
  let ls0 = NonEmpty.toList ls
  (idx, ls1) <- aux ls0
  (token, ls2) <- aux ls1
  (produce, _ls3) <- aux ls2
  pure $
    ParallelLexerMasks
    { tokenMask = mask token
    , tokenOffset = offset token
    , indexMask = mask idx
    , indexOffset = offset idx
    , producingMask = mask produce
    , producingOffset = offset produce
    }
  where
    aux = maybeToEither errorMessage . List.uncons

parallelLexerMasksToMasks64 :: ParallelLexerMasks -> Masks64
parallelLexerMasksToMasks64 lexer_masks =
  Masks64 $ NonEmpty.fromList elems
  where
    ParallelLexerMasks
      { indexMask = mask_index
      , indexOffset = offset_index
      , tokenMask = mask_token
      , tokenOffset = offset_token
      , producingMask = mask_produce
      , producingOffset = offset_produce
      } = lexer_masks
    elems =
      [Mask64 {mask = mask_index, offset = offset_index}
      ,Mask64 {mask = mask_token, offset = offset_token}
      ,Mask64 {mask = mask_produce, offset = offset_produce}]

lexerMasks :: ParallelLexer t k -> Either String ParallelLexerMasks
lexerMasks lexer = do
  ms <- parallelLexerToMasks64 lexer
  masks64ToParallelLexerMasks ms

encodeEndoData ::
  Ord k =>
  ParallelLexerMasks ->
  Map (Maybe k) Int ->
  ExtEndoData k ->
  Either String Int
encodeEndoData lexer_masks to_int endo_data = do
  t <- findInt maybe_token 
  encodeMasks64 ms $ NonEmpty.fromList [e, t, p]
  where
    ms = parallelLexerMasksToMasks64 lexer_masks
    ExtEndoData
      { endo = e
      , token = maybe_token
      , isProducing = produce
      } = endo_data
    p = fromEnum produce
    findInt = maybeToEither errorMessage . flip Map.lookup to_int


class (Semigroup t, Ord t, Ord s) => Sim t s where
  toState :: s -> t -> Maybe (Bool, s)

instance Sim ExtEndo S where
  toState s (ExtEndo endo producing) = do
    let (a, b) = bounds endo
    unless (a <= s && s <= b) Nothing
    pure (producing UArray.! s, endo UArray.! s)

toData ::
  (Sim t s, Ord k) =>
  s ->
  Map s k ->
  Set s ->
  E ->
  t ->
  ExtEndoData k
toData initial_state token_map accept_states e t =
  case toState initial_state t of
    Just (is_producing, s) ->
      ExtEndoData
      { endo = e
      , token = Map.lookup s token_map
      , isAccepting = s `Set.member` accept_states
      , isProducing = is_producing
      }
    Nothing -> deadEndoData

data IntParallelLexer t =
  IntParallelLexer
  { parLexer :: !(ParallelLexer t Int)
  , parMasks :: !ParallelLexerMasks
  } deriving (Show, Eq, Ord)

intParallelLexer ::
  (Sim t s, Ord k) =>
  Map (Maybe k) Int ->
  ParallelDFALexer t s k ->
  Either String (IntParallelLexer t)
intParallelLexer to_int lexer = do
  parallel_lexer <- parallelLexer lexer undefined
  ms <- lexerMasks parallel_lexer
  let encode = encodeEndoData ms to_int
  new_compositions <- mapM encode $ compositions parallel_lexer
  new_endomorphims <- mapM encode $ endomorphisms parallel_lexer
  new_identity <- encode $ identity parallel_lexer
  let new_parallel_lexer =
        parallel_lexer
        { compositions = new_compositions
        , endomorphisms = new_endomorphims
        , identity = new_identity
        }
  pure $
    IntParallelLexer
    { parLexer = new_parallel_lexer
    , parMasks = ms
    }
  
data EndoCtx t s k =
  EndoCtx
  { comps :: !(Map (E, E) E)
  , endoMap :: !(Map t E)
  , inverseEndoMap :: !(IntMap t)
  , endoData :: !(IntMap (ExtEndoData k))
  , connectedMap :: !(IntMap IntSet)
  , inverseConnectedMap :: !(IntMap IntSet)
  , maxE :: !E
  , ecInitialState :: !s
  , ecTokenMap :: !(Map s k)
  , ecAccepting :: !(Set s)
  } deriving (Show, Eq, Ord)

endoInsert ::
  (Ord t) =>
  E ->
  t ->
  EndoState t s k ()
endoInsert e endo = do
  new_inv_map <- IntMap.insert e endo <$> gets inverseEndoMap
  new_map <- Map.insert endo e <$> gets endoMap
  
  modify $
    \s ->
      s { inverseEndoMap = new_inv_map
        , endoMap = new_map }

eLookup :: E -> EndoState t s k (Maybe t)
eLookup e = IntMap.lookup e <$> gets inverseEndoMap

connectedLookup :: E -> EndoState t s k (Maybe IntSet)
connectedLookup e = IntMap.lookup e <$> gets connectedMap

connectedUpdate :: E -> IntSet -> EndoState t s k ()
connectedUpdate e e_set = do
  inv_map <- gets inverseConnectedMap
  new_map <- IntMap.insertWith IntSet.union e e_set <$> gets connectedMap
  let new_inverse_map =
        IntMap.unionWith IntSet.union inv_map
        $ IntMap.fromList
        $ (,IntSet.singleton e) <$> IntSet.toList e_set
  modify $ \s ->
    s { connectedMap = new_map
      , inverseConnectedMap = new_inverse_map }

connectedUpdateAll :: IntMap IntSet -> EndoState t s k ()
connectedUpdateAll =
  mapM_ (uncurry connectedUpdate) . IntMap.assocs

insertComposition :: E -> E -> E -> EndoState t s k ()
insertComposition e e' e'' = do
  modify $ \s -> s { comps = Map.insert (e, e') e'' $ comps s }

insertDeadIdentity :: E -> EndoState t s k ()
insertDeadIdentity e = do
  modify $ \s ->
    s { comps =
        Map.insert (e, identityEndo) e
        $ Map.insert (identityEndo, e) e 
        $ Map.insert (deadEndo, e) deadEndo 
        $ Map.insert (e, deadEndo) deadEndo 
        $ comps s
      }

preSets :: E -> E -> EndoState t s k (IntMap IntSet)
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

postSets :: E -> E -> EndoState t s k (IntMap IntSet)
postSets e'' e' = do
  e_set' <- fromMaybe IntSet.empty <$> connectedLookup e'
  e_set'' <- fromMaybe IntSet.empty <$> connectedLookup e''
  pure $ IntMap.singleton e'' (IntSet.difference e_set' e_set'')

endomorphismLookup ::
  (Ord t) =>
  t ->
  EndoState t s k (Maybe E)
endomorphismLookup endomorphism = do
  Map.lookup endomorphism <$> gets endoMap

instance Semigroup ExtEndo where
  (ExtEndo a a') <> (ExtEndo b b') = ExtEndo c c'
    where
      c = UArray.array (0, numElements a - 1)
        $ map auxiliary [0..(numElements a - 1)]
      c' = UArray.array (0, numElements a' - 1)
        $ map auxiliary' [0..(numElements a' - 1)]
      auxiliary i = (i, b UArray.! (a UArray.! i))
      auxiliary' i = (i, b' UArray.! (a UArray.! i))

endoToData ::
  (Sim t s, Ord k) =>
  E ->
  t ->
  EndoState t s k ()
endoToData e endo = do
  initial_state <- gets ecInitialState
  token_map <- gets ecTokenMap
  accept_states <- gets ecAccepting
  let d = toData initial_state token_map accept_states e endo
  modify (\s -> s { endoData = IntMap.insert e d $ endoData s })

endoNext :: (Sim t s, Ord k) => t -> EndoState t s k E
endoNext endo = do
  e <- succ <$> gets maxE
  insertDeadIdentity e
  endoToData e endo
  endoInsert e endo
  modify $ \s -> s { maxE = e }
  pure e

endoCompose ::
  (Sim t s, Ord k) =>
  E ->
  E ->
  EndoState t s k (IntMap IntSet)
endoCompose e e' = do
  maybe_endo <- eLookup e
  maybe_endo' <- eLookup e'
  case (maybe_endo, maybe_endo') of
    (Just endo, Just endo') -> do
      _comps <- gets comps
      case Map.lookup (e, e') _comps of
        Just _ -> pure IntMap.empty
        Nothing -> do
          let endo'' = endo <> endo'
          maybe_e'' <- endomorphismLookup endo''
          e'' <- maybe (endoNext endo'') pure maybe_e''
          insertComposition e e' e''
          pre_sets <- preSets e'' e
          post_sets <- postSets e'' e'
          let new_sets =
                IntMap.unionWith IntSet.union pre_sets post_sets
          connectedUpdateAll new_sets
          pure new_sets
    _any -> throwError errorMessage -- This should never happen.

popElement :: IntMap IntSet -> Maybe ((E, E), IntMap IntSet)
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
  (Sim t s, Ord k) =>
  EndoState t s k ()
endoCompositionsTable = gets connectedMap >>= auxiliary
  where
    auxiliary connected_map =
      case popElement connected_map of
        Just ((e, e'), map') -> do
          map'' <- endoCompose e e'
          let !map''' = IntMap.unionWith IntSet.union map' map''
          auxiliary map''' 
        Nothing -> pure ()

parallelLexer ::
  (Sim t s, Ord k) =>
  ParallelDFALexer t s k ->
  (t -> t') ->
  Either String (ParallelLexer t (ExtEndoData k))
parallelLexer lexer f =
  flip evalStateT s $ do
    endoCompositionsTable
    _compositions <- gets comps
    endo_data <- gets endoData
    let comps = fromMaybe deadEndoData . flip IntMap.lookup endo_data <$> _compositions
    endo_size <- gets (Map.size . endoMap)
    pure $
      ParallelLexer
      { compositions = comps
      , endomorphisms = t_to_data
      , tokenSize = token_size
      , endomorphismsSize = endo_size
      }
  where
    token_size = Map.size $ terminalMap $ parDFALexer lexer
    (t_to_data', s) = initEndoCtx lexer f
    unknown = Map.fromList $ map (,deadEndoData) [minBound..maxBound]
    t_to_data = t_to_data' `Map.union` unknown

endomorphismTable ::
  (IsTransition t, Ord k) =>
  ParallelDFALexer t S k ->
  Map t ExtEndo
endomorphismTable lexer =
  Map.fromList
  $ map statesFromChar
  $ Set.toList
  $ alphabet dfa
  where
    dfa = fsa $ parDFALexer lexer
    produces_set = producesToken lexer
    states_list = Set.toList $ states dfa
    states_size = Set.size $ states dfa
    state_to_int = Map.fromList $ zip states_list [initState..]
    stateToInt = (state_to_int Map.!)
    statesFromChar t = (t, ExtEndo ss bs)
      where
        ss = auxiliary $ stateToInt . fromMaybe deadState . uncurry (transition lexer) . (, t)
        bs = auxiliary $ (`Set.member` produces_set) . (, t)
        auxiliary f =
          UArray.array (0, states_size)
          $ zip [0..states_size]
          $ f <$> states_list

connectedTable ::
  (Ord s, Ord t) =>
  DFA t s -> Map t (Set t)
connectedTable dfa =
  Map.fromList
  $ auxiliary <$> _alphabet
  where
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

enumerate :: (Foldable f, Ord a) => Int -> f a -> Map a Int
enumerate a = Map.fromList . flip zip [a ..] . toList

invertBijection :: (Ord a, Ord b) => Map a b -> Map b a
invertBijection = Map.fromList . fmap swap . Map.assocs

enumerateByMap :: (Functor f, Ord a) => Map a Int -> f a -> f Int
enumerateByMap _map = fmap (_map Map.!)

enumerateKeysByMap :: Ord a => Map a Int -> Map a b -> Map Int b
enumerateKeysByMap _map = Map.mapKeys (_map Map.!)

intMapToMap :: IntMap a -> Map Int a
intMapToMap = Map.fromList . IntMap.toList

mapToIntMap :: Map Int a -> IntMap a
mapToIntMap = IntMap.fromList . Map.toList

setToIntSet :: Set Int -> IntSet
setToIntSet = IntSet.fromList . Set.toList

enumerateSetMapByMap :: Ord a => Map a Int -> Map a (Set a) -> IntMap IntSet
enumerateSetMapByMap _map =
  mapToIntMap
  . enumerateKeysByMap _map
  . fmap (setToIntSet
          . Set.fromList
          . enumerateByMap _map
          . Set.toList)

invertIntSetMap :: IntMap IntSet -> IntMap IntSet 
invertIntSetMap =
  IntMap.unionsWith IntSet.union
  . IntMap.mapWithKey toMap
  where
    toMap k =
      IntMap.fromList
      . fmap (,IntSet.singleton k)
      . IntSet.toList

initCompositions :: Map (E, E) E
initCompositions =
  Map.fromList [
    ((deadEndo, deadEndo), deadEndo),
    ((identityEndo, identityEndo), identityEndo),
    ((identityEndo, deadEndo), deadEndo),
    ((deadEndo, identityEndo), deadEndo)
  ]

initEndoCtx ::
  (Sim t' s, Ord k) =>
  ParallelDFALexer t s k ->
  (t -> t') ->
  (Map t (ExtEndoData k), EndoCtx t' s k)
initEndoCtx lexer f =
  (t_to_endo, 
   EndoCtx
   { comps = initCompositions
   , endoMap = endo_to_e
   , inverseEndoMap = e_to_endo
   , connectedMap = connected_table
   , inverseConnectedMap = inverse_connected_table
   , maxE = maximum endo_to_e
   , endoData = endo_data
   , ecInitialState = initial_state
   , ecTokenMap = token_map
   , ecAccepting = accept_states
   })
  where
    alpha = alphabet $ fsa $ parDFALexer lexer
    initial_state = initial $ fsa $ parDFALexer lexer
    token_map = terminalMap $ parDFALexer lexer
    accept_states = accepting $ fsa $ parDFALexer lexer
    endo_data = mapToIntMap $ enumerateKeysByMap t_to_e t_to_endo
    teToData t e = toData initial_state token_map accept_states e (f t) 
    t_to_endo = Map.mapWithKey teToData t_to_e
    t_to_e = enumerate initEndo alpha
    e_to_endo = mapToIntMap $ f <$> invertBijection t_to_e
    endo_to_e = invertBijection $ intMapToMap e_to_endo
    connected_table =
      enumerateSetMapByMap endo_to_e
      $ connectedTable
      $ fsaFirst f
      $ fsa
      $ parDFALexer lexer
    inverse_connected_table = invertIntSetMap connected_table

