module Alpacc.Lexer.DFA
  ( nfaFromRegEx,
    DFA (..),
    addDeadStateDFA,
    defaultTransitions,
    dfaFromRegEx,
    isMatch,
    isMatchPar,
    parallelLexingTable,
    overlappingTerminals
  )
where

import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Maybe (fromMaybe, fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple.Extra (both)
import Alpacc.Lexer.RegularExpression
import Alpacc.Lexer.NFA

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

nfaFromRegEx :: (Ord t, Show s, Ord s, Enum s) => s -> RegEx t -> NFA t s
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
  State (NFA t s) (Either String (Map (Set s, Char) (Set s)))
mkDFATransitions _ table [] = return $ Right table
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
    deadState :: Maybe s,
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
      deadState = f <$> deadState dfa,
      finalTerminalStates = Set.map f <$> finalTerminalStates dfa,
      initialTerminalStates = Set.map f <$> initialTerminalStates dfa,
      continueTerminalStates = Set.map f <$> continueTerminalStates dfa
    }

mkDFAFromNFA :: (Show s, Enum s, Ord s, Ord t) => State (NFA t s) (Either String (DFA t (Set s)))
mkDFAFromNFA = do
  nfa' <- get
  new_initial' <- epsilonClosure . Set.singleton $ initial' nfa'
  either_transitions' <- mkDFATransitions Set.empty Map.empty [new_initial']
  return $ constructDFA new_initial' nfa' either_transitions'
  where
    newTokenMap states' = Map.unionsWith Set.union . fmap (newStates states') . Map.toList
    newStates states' (((s, s'), c), t) =
      Map.fromList [(((a, b), c), t) | a <- newState' s, b <- newState' s']
      where
        newState' _s = toList $ Set.filter (Set.member _s) states'

    constructDFA new_initial nfa either_transitions = do
      new_transitions <- either_transitions
      let accept = accepting' nfa
      let token_map = terminalMap' nfa
      let (new_states, new_alphabet) = bimap Set.fromList Set.fromList . unzip $ Map.keys new_transitions
      let newStates' set = Set.filter (any (`Set.member` set)) new_states
      let new_accepting = newStates' accept
      let new_final_terminal_states = newStates' <$> finalTerminalStates' nfa
      let new_initial_terminal_states = newStates' <$> initialTerminalStates' nfa
      let new_continue_terminal_states = newStates' <$> continueTerminalStates' nfa
      return $
        if null new_transitions
          then
            DFA
              { states = Set.singleton Set.empty,
                alphabet = Set.empty,
                transitions = new_transitions,
                initial = Set.empty,
                accepting = Set.singleton Set.empty,
                terminalMap = Map.empty,
                deadState = Nothing,
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
                terminalMap = newTokenMap new_states token_map,
                deadState = Nothing,
                finalTerminalStates = new_final_terminal_states,
                initialTerminalStates = new_initial_terminal_states,
                continueTerminalStates = new_continue_terminal_states
              }

mkDFAFromRegEx :: (Ord t, Show s, Enum s, Ord s) => RegEx t -> State (NFA t s) (Either String (DFA t (Set s)))
mkDFAFromRegEx regex = do
  mkNFA regex
  mkDFAFromNFA

reenumerateDFA :: (Show s, Show s', Ord s, Enum s, Ord s') => s -> DFA t s' -> DFA t s
reenumerateDFA start_state dfa = dfaMap alphabetMap dfa
  where
    alphabet' = Map.fromList . flip zip [start_state ..] . toList $ states dfa
    alphabetMap = (alphabet' Map.!)

dfaFromRegEx :: (Ord t, Show s, Ord s, Enum s, Show t) => s -> RegEx t -> Either String (DFA t s)
dfaFromRegEx start_state regex = reenumerateDFA start_state <$> either_dfa
  where
    either_dfa = evalState (mkDFAFromRegEx regex) init_nfa
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
defaultTransitions DFA {deadState = Nothing} = Nothing
defaultTransitions DFA {states = _states, deadState = Just s} =
  Just $ map (,s) $ toList _states

parallelLexingTable :: (Ord s, Show s) => DFA t s -> Maybe (Map Char [(s, s)])
parallelLexingTable DFA {deadState = Nothing} = Nothing
parallelLexingTable
  DFA
    { transitions = _transitions,
      alphabet = _alphabet,
      states = _states,
      deadState = Just s
    } = Just table
    where
      tableLookUp key = fromMaybe s (Map.lookup key _transitions)
      statesFromChar a = (a,) $ map (\b -> (b, tableLookUp (b, a))) $ toList _states
      table = Map.fromList $ map statesFromChar $ toList _alphabet

addDeadStateDFA :: (Enum s, Ord s) => DFA t s -> DFA t s
addDeadStateDFA dfa =
  dfa
    { states = new_states,
      deadState = Just dead_state
    }
  where
    _states = states dfa
    dead_state = succ $ maximum _states
    new_states = Set.insert dead_state _states

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

overlappingTerminals :: (Ord s, Ord t) => DFA t s -> Set t
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
                Nothing -> error "Some transitions in the DFA is not associated with a terminal"

