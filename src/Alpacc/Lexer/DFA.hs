module Alpacc.Lexer.DFA
  ( DFA,
    isMatch,
    invertSetMap,
    lexerDFA,
    DFALexer,
    fromRegExToDFA,
    transitions',
    dfaLexerSpec,
    DFALexerSpec,
    dfaTerminals,
  )
where

import Alpacc.Lexer.FSA
import Alpacc.Lexer.NFA
import Alpacc.Util
import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable
import Data.Function
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Text (Text)
import Data.Text qualified as Text

type DFA t s = FSA Identity t s

type DFALexer t s k = Lexer Identity t s k

transitions' :: (Ord s, Ord t) => DFA t s -> Map (s, t) s
transitions' = fmap runIdentity . transitions

addIdentity :: (Ord s, Ord t) => Map (s, t) s -> Map (s, t) (Identity s)
addIdentity = fmap Identity

dfaTransitionEntry ::
  (Ord s, Ord t) =>
  Set s ->
  t ->
  State (NFA t s) (Map (Set s, t) (Set s))
dfaTransitionEntry set t = do
  _states <- statesTransitions set $ Trans t
  eps_states <- epsilonClosure _states
  pure $ Map.singleton (set, t) eps_states

dfaTransitionEntries ::
  (Ord s, Ord t) =>
  Set s ->
  State (NFA t s) (Map (Set s, t) (Set s))
dfaTransitionEntries set = do
  alph <- gets (fmap fromTransition . toList . alphabet)
  new_table_entry <- mapM (dfaTransitionEntry set) alph
  pure $ Map.unionsWith Set.union new_table_entry

dfaTransitions ::
  (Ord s, Ord t) =>
  Set (Set s) ->
  Map (Set s, t) (Set s) ->
  [Set s] ->
  State (NFA t s) (Map (Set s, t) (Set s))
dfaTransitions _ table [] = pure table
dfaTransitions visited table (top : queue) = do
  entries <- dfaTransitionEntries top
  let new_visited = Set.insert top visited
  let rest = Map.elems entries
  let new_queue = filter (`Set.notMember` new_visited) $ queue ++ rest
  let new_table = Map.unionWith Set.union entries table
  dfaTransitions new_visited new_table new_queue

dfaFilter :: (Ord s) => (s -> Bool) -> DFA t s -> DFA t s
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

emptyDFA :: DFA t (Set s)
emptyDFA =
  FSA
    { states = Set.singleton Set.empty,
      alphabet = Set.empty,
      transitions = Map.empty,
      initial = Set.empty,
      accepting = Set.empty
    }

fromNFAtoDFAState :: (Ord s, Ord t) => State (NFA t s) (DFA t (Set s))
fromNFAtoDFAState = do
  nfa <- get
  new_initial <- epsilonClosure . Set.singleton $ initial nfa
  new_transitions' <- dfaTransitions Set.empty Map.empty [new_initial]
  let new_transitions = addIdentity new_transitions'
  let accept = accepting nfa
  let (new_states, new_alphabet) =
        bimap Set.fromList Set.fromList $
          unzip $
            Map.keys new_transitions'
  let new_accepting = newStates new_states accept
  pure $
    if null new_transitions
      then emptyDFA
      else
        removeUselessStates $
          FSA
            { states = new_states,
              alphabet = new_alphabet,
              transitions = new_transitions,
              initial = new_initial,
              accepting = new_accepting
            }
  where
    newStates new_states' set = Set.filter (any (`Set.member` set)) new_states'

fromNFAtoDFA :: (Ord s, Ord t) => NFA t s -> DFA t (Set s)
fromNFAtoDFA = evalState fromNFAtoDFAState

fromRegExToDFA :: (Ord s, Ord t, Enum s) => s -> RegEx (NonEmpty t) -> DFA t (Set s)
fromRegExToDFA s = fromNFAtoDFA . fromRegExToNFA s

isMatch :: (Ord s) => DFA Char s -> Text -> Bool
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
removeUselessStates :: (Ord s, Ord t) => DFA t s -> DFA t s
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
dfaTotal :: (Ord s, Ord t, Enum s) => DFA t s -> (DFA t s, Maybe s)
dfaTotal dfa'
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
minimize :: (Ord s, Ord t, Enum s) => DFA t s -> DFA t (Set s)
minimize dfa' = removeUselessStates new_dfa
  where
    new_dfa = fsaSecond (state_map Map.!) dfa

    dfa = fst $ dfaTotal dfa'
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

dfaToNFA :: (Ord s, Ord t) => DFA t s -> NFA t s
dfaToNFA dfa =
  fsaFirst Trans $
    dfa {transitions = new_transitions}
  where
    new_transitions =
      Set.singleton <$> transitions' dfa

tokenProducingTransitions ::
  (Ord s, Ord t) =>
  DFA t s ->
  Map (s, t) s
tokenProducingTransitions dfa = new_transitions
  where
    accept = Set.toList $ accepting dfa
    _alphabet = Set.toList $ alphabet dfa
    _initial = initial dfa
    _transitions = transitions' dfa
    isInitialTransition = (`Map.member` _transitions) . (_initial,)
    initial_transitions = filter isInitialTransition _alphabet

    new_transitions =
      Map.unions $
        ( \q ->
            Map.fromList $
              mapMaybe (auxiliary q) initial_transitions
        )
          <$> accept

    auxiliary q t
      | (q, t) `Map.member` _transitions = Nothing
      | otherwise = do
          q' <- Map.lookup (_initial, t) _transitions
          pure ((q, t), q')

addProducingTransitions ::
  (Ord s, Ord t) =>
  DFA t s ->
  (DFA t s, Set (s, t))
addProducingTransitions dfa =
  (new_dfa, produces_token)
  where
    token_producing_trans = tokenProducingTransitions dfa
    _transitions = transitions' dfa
    produces_token = Map.keysSet token_producing_trans
    new_trans = Map.union _transitions token_producing_trans
    new_dfa = dfa {transitions = addIdentity new_trans}

data DFALexerSpec t o k
  = DFALexerSpec
  { orderMap :: Map k o,
    regexMap :: Map k (RegEx (NonEmpty t))
  }
  deriving (Show, Ord, Eq)

dfaTerminals :: DFALexerSpec t o k -> [k]
dfaTerminals = Map.keys . orderMap

dfaLexerSpec :: (Ord o, Ord k, Enum o) => o -> [(k, RegEx (NonEmpty t))] -> DFALexerSpec t o k
dfaLexerSpec start t_rules =
  DFALexerSpec order_map terminal_map
  where
    order_map = Map.fromList $ flip zip [start ..] $ fst <$> t_rules
    terminal_map = Map.fromList t_rules

lexerDFA ::
  (Ord t, Ord s, Enum s, Ord k, Ord o) =>
  s ->
  DFALexerSpec t o k ->
  DFALexer t s k
lexerDFA start_state spec =
  enumerateLexer start_state $
    Lexer
      { fsa = dfa,
        tokenMap = dfa_token_map,
        producesToken = produces_token
      }
  where
    terminal_to_order = orderMap spec
    regex_map = regexMap spec

    auxiliary =
      dfaToNFA
        . enumerateFSA start_state
        . minimize
        . enumerateFSA start_state
        . fromNFAtoDFA
        . fromRegExToNFA start_state

    dfa_map' = auxiliary <$> regex_map
    nfa_map = enumerateFSAsMap start_state dfa_map'
    nfas = Map.elems nfa_map
    initials = Set.fromList $ initial <$> nfas

    newStates set k s =
      Map.unionsWith Set.union
        . map
          ( uncurry Map.singleton
              . (,Set.singleton k)
          )
        . Set.toList
        $ Set.filter (s `Set.member`) set

    minimumSet = minimumBy (on compare (terminal_to_order Map.!))

    dfa_token_map =
      fmap minimumSet
        . Map.unionsWith Set.union
        . concat
        . Map.elems
        $ Map.mapWithKey
          ( \k ->
              map (newStates (states dfa) k)
                . Set.toList
                . accepting
          )
          nfa_map

    new_states' = Set.unions $ states <$> nfas
    new_initial = succ $ maximum $ Set.insert start_state new_states'
    new_states = Set.insert new_initial new_states'
    new_alphabet = Set.unions $ alphabet <$> nfas
    new_accepting = Set.unions $ accepting <$> nfas

    new_transitions =
      Map.insert (new_initial, Eps) initials $
        Map.unionsWith Set.union $
          transitions <$> nfas

    (dfa, produces_token) =
      addProducingTransitions $
        fromNFAtoDFA $
          FSA
            { states = new_states,
              alphabet = new_alphabet,
              transitions = new_transitions,
              accepting = new_accepting,
              initial = new_initial
            }
