module Alpacc.Lexer.DFA
  ( DFA,
    isMatch,
    parallelLexingTable,
    invertSetMap,
    lexerDFA,
    DFALexer,
    fromRegExToDFA,
  )
where

import Alpacc.Lexer.FSA
import Alpacc.Lexer.NFA
import Alpacc.Util
import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.Function
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Text (Text)
import Data.Text qualified as Text

type DFA t s = FSA Identity Identity t s

type DFALexer t s k = Lexer Identity Identity t s k

transitions' :: (IsState s, IsTransition t) => DFA t s -> Map (s, t) s
transitions' = Map.mapKeys (second runIdentity) . fmap runIdentity . transitions

addIdentity :: (IsState s, IsTransition t) => Map (s, t) s -> Map (s, Identity t) (Identity s)
addIdentity = Map.mapKeys (second Identity) . fmap Identity

stateTransitions :: (IsState s, IsTransition t) => Transition t -> s -> State (NFA t s) (Set s)
stateTransitions c s = do
  nfa <- get
  let trans = transitions nfa
  let eps_map = Map.filterWithKey (\k _ -> isSymbolTransition k) trans
  return . Set.unions $ toList eps_map
  where
    isSymbolTransition (s', c') = s == s' && c == c'

epsilonTransitions :: (IsState s, IsTransition t) => s -> State (NFA t s) (Set s)
epsilonTransitions = stateTransitions Eps

statesTransitions :: (IsState s, IsTransition t) => Set s -> Transition t -> State (NFA t s) (Set s)
statesTransitions set c = Set.unions <$> mapM (stateTransitions c) (toList set)

epsilonClosure :: (IsState s, IsTransition t) => Set s -> State (NFA t s) (Set s)
epsilonClosure set = do
  new_set <- Set.unions <$> mapM epsilonTransitions (toList set)
  let set' = new_set `Set.union` set
  if set == set'
    then return set'
    else epsilonClosure set'

mkDFATransitionEntry ::
  (IsState s, IsTransition t) =>
  Set s ->
  t ->
  State (NFA t s) (Map (Set s, t) (Set s))
mkDFATransitionEntry set t = do
  _states <- statesTransitions set $ Trans t
  eps_states <- epsilonClosure _states
  return $ Map.singleton (set, t) eps_states

mkDFATransitionEntries ::
  (IsState s, IsTransition t) =>
  Set s ->
  State (NFA t s) (Map (Set s, t) (Set s))
mkDFATransitionEntries set = do
  alph <- gets (toList . alphabet)
  new_table_entry <- mapM (mkDFATransitionEntry set) alph
  return $ Map.unionsWith Set.union new_table_entry

mkDFATransitions ::
  (IsState s, IsTransition t) =>
  Set (Set s) ->
  Map (Set s, t) (Set s) ->
  [Set s] ->
  State (NFA t s) (Map (Set s, t) (Set s))
mkDFATransitions _ table [] = return table
mkDFATransitions visited table (top : queue) = do
  entries <- mkDFATransitionEntries top
  let new_visited = Set.insert top visited
  let rest = Map.elems entries
  let new_queue = filter (`Set.notMember` new_visited) $ queue ++ rest
  let new_table = Map.unionWith Set.union entries table
  mkDFATransitions new_visited new_table new_queue

dfaFilter :: Ord s => (s -> Bool) -> DFA t s -> DFA t s
dfaFilter p dfa =
  dfa
    { states = Set.filter p (states dfa),
      transitions = Map.filterWithKey (\(s, _) s' -> p s && p' s') (transitions dfa),
      initial = _initial,
      accepting = p `Set.filter` accepting dfa
    }
  where
    p' = p . runIdentity
    _initial =
      if p $ initial dfa
        then initial dfa
        else error "Can not filter states since the initial state is removed."

solveTerminalMapping ::
  (IsState s, IsTransition t, Ord k) =>
  Map ((s, s), t) (Set k) ->
  ((Set s, t), Set s) ->
  (((Set s, Set s), t), Set k)
solveTerminalMapping terminal_map ((s, c), s') = (((s, s'), c), new_set)
  where
    lookup' = (`Map.lookup` terminal_map)
    new_set =
      Set.unions $
        catMaybes [lookup' ((x, y), c) | x <- toList s, y <- toList s']

solveTerminalMap ::
  (IsState s, IsTransition t, Ord k) =>
  Map ((s, s), t) (Set k) ->
  Map (Set s, t) (Set s) ->
  Map ((Set s, Set s), t) (Set k)
solveTerminalMap terminal_map =
  Map.filter (not . null)
    . Map.fromList
    . fmap (solveTerminalMapping terminal_map)
    . Map.toList

fromNFAtoDFAState :: (IsState s, IsTransition t) => State (NFA t s) (DFA t (Set s))
fromNFAtoDFAState = do
  nfa <- get
  new_initial <- epsilonClosure . Set.singleton $ initial nfa
  new_transitions' <- mkDFATransitions Set.empty Map.empty [new_initial]
  let new_transitions = addIdentity new_transitions'
  let accept = accepting nfa
  let (new_states, new_alphabet) = bimap Set.fromList Set.fromList . unzip $ Map.keys new_transitions'
  let new_accepting = newStates new_states accept
  return $
    removeUselessStates $
      if null new_transitions
        then
          FSA
            { states = Set.singleton Set.empty,
              alphabet = Set.empty,
              transitions = new_transitions,
              initial = Set.empty,
              accepting = Set.singleton Set.empty
            }
        else
          FSA
            { states = new_states,
              alphabet = new_alphabet,
              transitions = new_transitions,
              initial = new_initial,
              accepting = new_accepting
            }
  where
    newStates new_states' set = Set.filter (any (`Set.member` set)) new_states'

fromNFAtoDFA :: (IsState s, IsTransition t) => NFA t s -> DFA t (Set s)
fromNFAtoDFA = evalState fromNFAtoDFAState

fromRegExToDFA :: (IsState s, IsTransition t, Enum s) => s -> RegEx (NonEmpty t) -> DFA t (Set s)
fromRegExToDFA s = fromNFAtoDFA . fromRegExToNFA s

isMatch :: (IsState s) => DFA Char s -> Text -> Bool
isMatch dfa = runDFA' start_state
  where
    start_state = initial dfa
    trans = transitions' dfa
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

parallelLexingTable :: (IsState s, IsTransition t, Enum s) => DFALexer t s k -> Map t [s]
parallelLexingTable lexer = table
  where
    dfa = fsa lexer
    _transitions = transitions' dfa
    _states = states dfa
    _alphabet = alphabet dfa
    tableLookUp key = _transitions Map.! key
    statesFromChar a =
      (a,) $
        map (\b -> tableLookUp (b, a)) $
          toList _states
    table = Map.fromList $ map statesFromChar $ toList _alphabet

invertSetMap :: (Ord t, Ord s) => Map t (Set s) -> Map s (Set t)
invertSetMap mapping = Map.fromList $ setMap <$> codomain
  where
    codomain = toList $ Set.unions mapping
    domain = Map.keys mapping
    setMap s =
      (s,) $
        Set.fromList $
          filter ((s `Set.member`) . (mapping Map.!)) domain

-- | http://www.cs.um.edu.mt/gordon.pace/Research/Software/Relic/Transformations/FSA/remove-useless.html
removeUselessStates :: (IsState s, IsTransition t) => DFA t s -> DFA t s
removeUselessStates dfa = dfaFilter (`Set.member` useful_states) dfa
  where
    initial_useful = accepting dfa
    _states = states dfa
    empty_map = Map.fromList $ (,Set.empty) <$> toList _states
    graph =
      Map.unionWith Set.union empty_map $
        Map.unionsWith Set.union $
          uncurry Map.singleton
            . bimap fst Set.singleton
            <$> Map.toList (transitions' dfa)
    newUsefulState s = Set.filter ((s `Set.member`) . (graph Map.!)) _states
    usefulStates s = Set.union s . Set.unions $ Set.map newUsefulState s
    useful_states = fixedPointIterate (/=) usefulStates initial_useful

-- | http://www.cs.um.edu.mt/gordon.pace/Research/Software/Relic/Transformations/FSA/to-total.html
mkDFATotal :: (IsState s, IsTransition t, Enum s) => DFA t s -> (DFA t s, Maybe s)
mkDFATotal dfa'
  | null $ states dfa' = (dfa', Nothing)
  | otherwise = (new_dfa, Just dead_state)
  where
    _states' = states dfa'
    dfa = dfa' {states = Set.insert dead_state _states'}
    new_dfa =
      dfa {transitions = new_transitions}
    new_transitions = addIdentity $ Map.union _transitions missing_transitions
    _states = states dfa
    _alphabet = alphabet dfa
    _transitions = transitions' dfa
    dead_state = succ $ Set.findMax _states'
    missing_transitions =
      Map.fromList $
        fmap (,dead_state) $
          concatMap missingStateTransitions $
            toList _states
    missingStateTransitions s =
      filter
        (`Map.notMember` _transitions)
        ((s,) <$> toList _alphabet)

-- | http://www.cs.um.edu.mt/gordon.pace/Research/Software/Relic/Transformations/FSA/minimise.html
minimize :: (IsState s, IsTransition t, Enum s) => DFA t s -> DFA t (Set s)
minimize dfa' = removeUselessStates new_dfa
  where
    new_dfa = fsaSecond (state_map Map.!) dfa

    dfa = fst $ mkDFATotal dfa'
    states_list = toList $ states dfa
    alphabet_list = toList (alphabet dfa)
    _transitions = transitions' dfa
    _accepting = accepting dfa
    isAccepting = flip Set.member _accepting
    initMatrixValue s s' = ((s, s'), ((/=) `on` isAccepting) s s')
    initial_matrix =
      Map.fromList [initMatrixValue s s' | s <- states_list, s' <- states_list]

    paths s = filter (\c -> (s, c) `Map.member` _transitions)
    commonPath s s' = paths s' $ paths s alphabet_list
    isDistinguishable matrix s s' =
      any (matrix Map.!) $
        zip (toStates s) (toStates s')
      where
        common_path = commonPath s s'
        toStates s'' = (_transitions Map.!) . (s'',) <$> common_path

    current_state_map =
      Map.fromList $
        (\s -> (s, Set.singleton s))
          <$> toList (states dfa)

    joinStates _states _ True = _states
    joinStates _states (s, s') False =
      Map.insert s' new_state $
        Map.insert s new_state _states
      where
        new_state = (_states Map.! s) `Set.union` (_states Map.! s')

    state_map = Map.foldlWithKey joinStates current_state_map final_matrix

    final_matrix = fixedPointIterate (/=) newMatrix initial_matrix
    newMatrix matrix = Map.mapWithKey (\k _ -> newMatrixValue matrix k) matrix
    newMatrixValue matrix st@(s, s') = matrix Map.! st || isDistinguishable matrix s s'

addDeadState :: (IsTransition t, IsState s, Enum s, Ord k) => DFALexer t s k -> DFALexer t s k
addDeadState lexer =
  lexer
    { fsa = dfa,
      deadState = dead_state
    }
  where
    (dfa, dead_state) = mkDFATotal $ fsa lexer

newSets :: Ord a => FSA f f' t (Set a) -> Set a -> Set (Set a)
newSets dfa set = Set.filter (not . null . Set.intersection set) _states
  where
    _states = states dfa

minimizeDFALexer :: (IsTransition t, IsState s, Enum s, Ord k) => s -> DFALexer t s k -> DFALexer t s k
minimizeDFALexer start_state lexer =
  addDeadState $
    reenumerateLexer start_state $
      Lexer
        { fsa = dfa,
          finalMap = new_final_map,
          terminalMap = new_terminal_map,
          deadState = Nothing
        }
  where
    dfa = minimize $ fsa lexer
    _transitions = transitions' dfa
    final_map = finalMap lexer
    terminal_map = Map.mapKeys (second runIdentity) $ terminalMap lexer

    new_final_map = newSets dfa <$> final_map
    new_terminal_map =
      Map.mapKeys (second Identity) $
        solveTerminalMap terminal_map _transitions

lexerDFA :: (IsTransition t, IsState s, Enum s, Ord k) => s -> Map k (RegEx (NonEmpty t)) -> DFALexer t s k
lexerDFA start_state regex_map =
  minimizeDFALexer start_state $
    reenumerateLexer start_state $
      Lexer
        { fsa = dfa,
          finalMap = dfa_final_map,
          terminalMap = dfa_terminal_map,
          deadState = Nothing
        }
  where
    nfa_map = fromRegExToNFA start_state <$> regex_map
    nfa_lexer = lexerNFA start_state nfa_map
    nfa = fsa nfa_lexer
    final_map = finalMap nfa_lexer
    terminal_map =
      Map.mapKeys (second fromTransition) $
        Map.filterWithKey (\(_, t) _ -> isTransition t) $
          terminalMap nfa_lexer

    dfa = fromNFAtoDFA nfa
    _transitions = transitions' dfa
    dfa_final_map = newSets dfa <$> final_map
    dfa_terminal_map =
      Map.mapKeys (second Identity) $
        solveTerminalMap terminal_map _transitions

