module Alpacc.Lexer.DFA
  ( nfaFromRegEx,
    DFA (..),
    addDeadStateDFA,
    defaultTransitions,
    dfaFromRegEx,
    isMatch,
    isMatchPar,
    parallelLexingTable,
    overlappingTerminals,
    invertSetMap
  )
where

import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Maybe (fromMaybe, fromJust, mapMaybe, catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple.Extra (both)
import Alpacc.Lexer.RegularExpression
import Alpacc.Lexer.NFA
import Alpacc.Util
import Data.Function

stateTransitions :: (Show s, Ord s) => Maybe Char -> s -> State (NFA t s) (Set s)
stateTransitions c s = do
  nfa <- get
  let trans = transitions' nfa
  let eps_map = Map.filterWithKey (\k _ -> isSymbolTransition k) trans
  return . Set.unions $ toList eps_map
  where
    isSymbolTransition (s', c') = s == s' && c == c'

epsilonTransitions :: (Show s, Ord s) => s -> State (NFA t s) (Set s)
epsilonTransitions = stateTransitions Nothing

statesTransitions :: (Show s, Ord s) => Set s -> Maybe Char -> State (NFA t s) (Set s)
statesTransitions set c = Set.unions <$> mapM (stateTransitions c) (toList set)

epsilonClosure :: (Show s, Ord s) => Set s -> State (NFA t s) (Set s)
epsilonClosure set = do
  new_set <- Set.unions <$> mapM epsilonTransitions (toList set)
  let set' = new_set `Set.union` set
  if set == set'
    then return set'
    else epsilonClosure set'

nfaFromRegEx :: (Ord t, Show s, Ord s, Enum s, Show t) => s -> RegEx t -> NFA t s
nfaFromRegEx start_state regex = execState (mkNFA regex) init_nfa
  where
    init_nfa = initNFA start_state

mkDFATransitionEntry ::
  (Show s, Ord s, Enum s) =>
  Set s ->
  Char ->
  State (NFA t s) (Map (Set s, Char) (Set s))
mkDFATransitionEntry set c = do
  _states <- statesTransitions set $ Just c
  eps_states <- epsilonClosure _states
  return $ Map.singleton (set, c) eps_states

mkDFATransitionEntries ::
  (Show s, Ord s, Enum s) =>
  Set s ->
  State (NFA t s) (Map (Set s, Char) (Set s))
mkDFATransitionEntries set = do
  alph <- gets (toList . alphabet')
  new_table_entry <- mapM (mkDFATransitionEntry set) alph
  return $ Map.unionsWith Set.union new_table_entry

mkDFATransitions ::
  (Show s, Ord s, Enum s) =>
  Set (Set s) ->
  Map (Set s, Char) (Set s) ->
  [Set s] ->
  State (NFA t s) (Map (Set s, Char) (Set s))
mkDFATransitions _ table [] = return table
mkDFATransitions visited table (top : queue) = do
  entries <- mkDFATransitionEntries top
  let new_visited = Set.insert top visited
  let rest = Map.elems entries
  let new_queue = filter (`Set.notMember` new_visited) $ queue ++ rest
  let new_table = Map.unionWith Set.union entries table
  mkDFATransitions new_visited new_table new_queue

data DFA t s = DFA
  { states :: Set s,
    alphabet :: Set Char,
    transitions :: Map (s, Char) s,
    initial :: s,
    accepting :: Set s,
    unreachableState :: Maybe s,
    terminalMap :: Map ((s, s), Char) (Set t),
    finalTerminalStates :: Map t (Set s),
    initialTerminalStates :: Map t (Set s),
    continueTerminalStates :: Map t (Set s)
  }
  deriving (Eq, Show)

dfaMap :: (Ord s', Ord s) => (s' -> s) -> DFA t s' -> DFA t s
dfaMap f dfa =
  dfa
    { states = Set.map f (states dfa),
      transitions = f <$> Map.mapKeys (first f) (transitions dfa),
      initial = f $ initial dfa,
      accepting = f `Set.map` accepting dfa,
      terminalMap = Map.mapKeys (first (both f)) $ terminalMap dfa,
      unreachableState = f <$> unreachableState dfa,
      finalTerminalStates = Set.map f <$> finalTerminalStates dfa,
      initialTerminalStates = Set.map f <$> initialTerminalStates dfa,
      continueTerminalStates = Set.map f <$> continueTerminalStates dfa
    }

dfaFilter :: Ord s => (s -> Bool) -> DFA t s -> DFA t s
dfaFilter p dfa =
  dfa
    { states = Set.filter p (states dfa),
      transitions = Map.filterWithKey (\(s,_) s' -> p s && p s') (transitions dfa),
      initial = _initial,
      accepting = p `Set.filter` accepting dfa,
      terminalMap =
        Map.filterWithKey (\((s, s'), _) _ -> p s && p s')
        $ terminalMap dfa,
      unreachableState = unreachable_state,
      finalTerminalStates = Set.filter p <$> finalTerminalStates dfa,
      initialTerminalStates = Set.filter p <$> initialTerminalStates dfa,
      continueTerminalStates = Set.filter p <$> continueTerminalStates dfa
    }
  where
    _initial =
      if p $ initial dfa
      then initial dfa
      else error "Can not filter states since the initial state is removed."
    unreachable_state =
      case unreachableState dfa of
        Just s -> if p s then Just s else Nothing
        Nothing -> Nothing

solveTerminalMapping ::
  (Ord s, Ord t) =>
  Map ((s, s), Char) (Set t) ->
  ((Set s, Char), Set s) ->
  (((Set s, Set s), Char), Set t)
solveTerminalMapping terminal_map ((s, c), s') = (((s, s'), c), new_set)
  where
    lookup' = (`Map.lookup` terminal_map)
    new_set =
      Set.unions
      $ catMaybes [lookup' ((x, y), c) | x <- toList s, y <- toList s']

solveTerminalMap ::
  (Ord s, Ord t) =>
  Map ((s, s), Char) (Set t) ->
  Map (Set s, Char) (Set s) ->
  Map ((Set s, Set s), Char) (Set t)
solveTerminalMap terminal_map =
  Map.filter (not . null)
  . Map.fromList
  . fmap (solveTerminalMapping terminal_map)
  . Map.toList

mkDFAFromNFA :: (Show s, Enum s, Ord s, Ord t, Show t) => State (NFA t s) (DFA t (Set s))
mkDFAFromNFA = do
  nfa <- get
  new_initial <- epsilonClosure . Set.singleton $ initial' nfa
  new_transitions <- mkDFATransitions Set.empty Map.empty [new_initial]
  let accept = accepting' nfa
  let (new_states, new_alphabet) = bimap Set.fromList Set.fromList . unzip $ Map.keys new_transitions
  let new_accepting = newStates new_states accept
  let new_final_terminal_states = newStates new_states <$> finalTerminalStates' nfa
  let new_initial_terminal_states = newStates new_states <$> initialTerminalStates' nfa
  let new_continue_terminal_states = newStates new_states <$> continueTerminalStates' nfa
  let terminal_map = terminalMap' nfa
  let new_terminal_map = solveTerminalMap terminal_map new_transitions
  return
    $ if null new_transitions
      then
        DFA
          { states = Set.singleton Set.empty,
            alphabet = Set.empty,
            transitions = new_transitions,
            initial = Set.empty,
            accepting = Set.singleton Set.empty,
            terminalMap = Map.empty,
            unreachableState = Nothing,
            finalTerminalStates = Map.empty,
            initialTerminalStates = Map.empty,
            continueTerminalStates = Map.empty
          }
      else
        DFA
          { states = new_states,
            alphabet = new_alphabet,
            transitions = new_transitions,
            initial = new_initial,
            accepting = new_accepting,
            terminalMap = new_terminal_map,
            unreachableState = Nothing,
            finalTerminalStates = new_final_terminal_states,
            initialTerminalStates = new_initial_terminal_states,
            continueTerminalStates = new_continue_terminal_states
          }
  where
    newStates new_states' set = Set.filter (any (`Set.member` set)) new_states'

mkDFAFromRegEx :: (Ord t, Show s, Enum s, Ord s, Show t) => RegEx t -> State (NFA t s) (DFA t (Set s))
mkDFAFromRegEx regex = do
  mkNFA regex
  mkDFAFromNFA

reenumerateDFA :: (Show s, Show s', Ord s, Enum s, Ord s', Show t) => s -> DFA t s' -> DFA t s
reenumerateDFA start_state dfa = dfaMap alphabetMap dfa
  where
    alphabet' = Map.fromList . flip zip [start_state ..] . toList $ states dfa
    alphabetMap = (alphabet' Map.!)

dfaFromRegEx :: (Ord t, Show s, Ord s, Enum s, Show t) => s -> RegEx t -> DFA t s
dfaFromRegEx start_state regex =
  reenumerateDFA start_state
  $ minimize
  $ reenumerateDFA start_state dfa
  where
    dfa = evalState (mkDFAFromRegEx regex) init_nfa
    init_nfa = initNFA 0 :: NFA t Integer

isMatch :: Ord s => DFA t s -> Text -> Bool
isMatch dfa = runDFA' start_state
  where
    start_state = initial dfa
    trans = transitions dfa
    runDFA' s str' =
      if Text.null str'
        then s `Set.member` accepting dfa
        else case maybe_state of
          Just state' -> runDFA' state' xs
          Nothing -> False
      where
        x = Text.head str'
        xs = Text.tail str'
        maybe_state = Map.lookup (s, x) trans

defaultTransitions :: DFA t s -> Maybe [(s, s)]
defaultTransitions DFA {unreachableState = Nothing} = Nothing
defaultTransitions DFA {states = _states, unreachableState = Just s} =
  Just $ map (,s) $ toList _states

parallelLexingTable :: (Ord s, Show s) => DFA t s -> Maybe (Map Char [(s, s)])
parallelLexingTable DFA {unreachableState = Nothing} = Nothing
parallelLexingTable
  DFA
    { transitions = _transitions,
      alphabet = _alphabet,
      states = _states,
      unreachableState = Just s
    } = Just table
    where
      tableLookUp key = fromMaybe s (Map.lookup key _transitions)
      statesFromChar a = (a,) $ map (\b -> (b, tableLookUp (b, a))) $ toList _states
      table = Map.fromList $ map statesFromChar $ toList _alphabet

addDeadStateDFA :: (Enum s, Ord s) => DFA t s -> DFA t s
addDeadStateDFA dfa =
  dfa
    { states = new_states,
      unreachableState = Just unreachable_state
    }
  where
    _states = states dfa
    unreachable_state = succ $ maximum _states
    new_states = Set.insert unreachable_state _states

isMatchPar :: DFA t Int -> Text -> Bool
isMatchPar dfa' str = last final_state `Set.member` accepting dfa
  where
    dfa = addDeadStateDFA dfa'
    str' = Text.unpack str
    _initial = initial dfa
    table = fromJust $ parallelLexingTable dfa
    default_case = fromJust $ defaultTransitions dfa
    combineTransitions (a, _) (_, b) = (a, b)
    zipper = zipWith combineTransitions
    tableLookUp key = fromMaybe default_case (Map.lookup key table)
    paths = map (map snd) $ scanl1 zipper $ map tableLookUp str'
    final_state = scanl (flip (List.!!)) _initial paths

-- | Not sure if this is needed, but if so this can be done faster with.
-- http://www.cs.um.edu.mt/gordon.pace/Research/Software/Relic/Transformations/FSA/intersection.html
overlappingTerminals :: (Ord s, Ord t, Show s, Show t) => DFA t s -> Set t
overlappingTerminals dfa = dfs 0 Set.empty Set.empty ne start
  where
    terminal_map = terminalMap dfa
    ne = Set.unions terminal_map
    accept = accepting dfa
    graph = transitions dfa
    start = initial dfa
    alpha = toList $ alphabet dfa
    edge (s, c) = do
      s' <- Map.lookup (s, c) graph
      return ((s, s'), c)
    notVisted visited = filter (`Set.notMember` visited)
    edges s = mapMaybe (edge . (s,)) alpha

    dfs (d :: Int) overlaps visited ts s
      | ts `Set.isSubsetOf` overlaps = Set.empty -- A overlap has already been found for the terminals ts.
      | Set.size ts <= 1 = Set.empty -- No overlap will occour on paths which contain this subpath.
      | otherwise = Set.union new_overlaps . Set.unions $ explore <$> _edges -- Explore.
      where
        new_overlaps =
          if s `Set.member` accept && d /= 0 -- If the set of terminals is not empty or a singleton and the state can be accepted then there is a overlap.
          then overlaps `Set.union` ts
          else overlaps

        _edges = notVisted visited $ edges s

        explore trans@((_, s'), _) = dfs (d + 1) new_overlaps new_visited new_ts s'
          where
            new_visited = Set.insert trans visited -- Do not walk back on an already explored transition.
            new_ts =
              case trans `Map.lookup` terminal_map of
                Just a -> a `Set.intersection` ts
                Nothing -> error "Some transitions in the DFA is not associated with a terminal."

invertSetMap :: (Ord t, Ord s) => Map t (Set s) -> Map s (Set t)
invertSetMap mapping = Map.fromList $ setMap <$> codomain
  where
    codomain = toList $ Set.unions mapping
    domain = Map.keys mapping
    setMap s =
      (s,)
      $ Set.fromList
      $ filter ((s `Set.member`) . (mapping Map.!)) domain

-- | http://www.cs.um.edu.mt/gordon.pace/Research/Software/Relic/Transformations/FSA/remove-useless.html
removeUselessStates :: (Ord s, Show s, Show t) => DFA t s -> DFA t s
removeUselessStates dfa = dfaFilter (`Set.member` useful_states) dfa
  where
    initial_useful = accepting dfa
    _states = states dfa
    empty_map = Map.fromList $ (,Set.empty) <$> toList _states
    graph =
      Map.unionWith Set.union empty_map
      $ Map.unionsWith Set.union
      $ uncurry Map.singleton
      . bimap fst Set.singleton
      <$> Map.toList (transitions dfa)
    newUsefulState s = Set.filter ((s `Set.member`) . (graph Map.!)) _states
    usefulStates s = Set.union s . Set.unions $ Set.map newUsefulState s
    useful_states = fixedPointIterate (/=) usefulStates initial_useful

-- | http://www.cs.um.edu.mt/gordon.pace/Research/Software/Relic/Transformations/FSA/to-total.html
mkDFATotal :: (Ord s, Show s, Show t, Enum s) => DFA t s -> DFA t s
mkDFATotal dfa'
  | null $ states dfa' = dfa'
  | otherwise = new_dfa
  where
    _states' = states dfa'
    dfa = dfa' {states = Set.insert dead_state _states' }
    new_dfa = dfa {transitions = Map.union _transitions missing_transitions }
    _states = states dfa
    _alphabet = alphabet dfa
    _transitions = transitions dfa
    dead_state = succ $ Set.findMax _states'
    missing_transitions =
      Map.fromList
      $ fmap (,dead_state)
      $ concatMap missingStateTransitions
      $ toList _states
    missingStateTransitions s =
      filter (`Map.notMember` _transitions)
      ((s,) <$> toList _alphabet)

-- | http://www.cs.um.edu.mt/gordon.pace/Research/Software/Relic/Transformations/FSA/minimise.html
minimize :: (Ord s, Show s, Show t, Enum s, Ord t) => DFA t s -> DFA t (Set s)
minimize dfa' = removeUselessStates $ new_dfa {terminalMap = new_terminal_map}
  where
    new_dfa = dfaMap (state_map Map.!) dfa
    terminal_map = terminalMap dfa
    new_terminal_map = solveTerminalMap terminal_map $ transitions new_dfa
    
    dfa = mkDFATotal dfa'
    states_list = toList $ states dfa
    alphabet_list = toList $ alphabet dfa
    _transitions = transitions dfa
    _accepting = accepting dfa
    isAccepting = flip Set.member _accepting
    initMatrixValue s s' = ((s, s'), ((/=) `on` isAccepting) s s')
    initial_matrix =
      Map.fromList [initMatrixValue s s' | s <- states_list, s' <- states_list]

    paths s = filter (\c -> (s, c) `Map.member` _transitions)
    commonPath s s' = paths s' $ paths s alphabet_list
    isDistinguishable matrix s s' =
      any (matrix Map.!)
      $ zip (toStates s) (toStates s')
      where
        common_path = commonPath s s'
        toStates s'' = (_transitions Map.!) . (s'',) <$> common_path

    current_state_map =
      Map.fromList
      $ (\s -> (s, Set.singleton s))
      <$> toList (states dfa)
    
    joinStates _states _ True = _states
    joinStates _states (s, s') False =
      Map.insert s' new_state
      $ Map.insert s new_state _states
      where
        new_state = (_states Map.! s) `Set.union` (_states Map.! s')
    
    state_map = Map.foldlWithKey joinStates current_state_map final_matrix

    final_matrix = fixedPointIterate (/=) newMatrix initial_matrix
    newMatrix matrix = Map.mapWithKey (\k _ -> newMatrixValue matrix k) matrix
    newMatrixValue matrix st@(s, s') = matrix Map.! st || isDistinguishable matrix s s'