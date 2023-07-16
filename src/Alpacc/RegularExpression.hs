module Alpacc.RegularExpression
  ( regExFromText,
    pRegEx,
    dfaFromRegEx,
    nfaFromRegEx,
    isMatch,
    DFA (..),
  )
where

import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isAlphaNum)
import Data.Foldable (Foldable (..))
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

data RegEx
  = Epsilon
  | Literal Char
  | Star RegEx
  | Alter RegEx RegEx
  | Concat RegEx RegEx
  deriving (Eq, Show)

space :: Parser ()
space = Lexer.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

pLiteral :: Parser RegEx
pLiteral = Literal <$> lexeme (satisfy (`elem` ['a' .. 'z'] ++ ['0' .. '9']))

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x =
      do
        f <- op
        y <- p
        rest (f x y)
        <|> return x

many1 :: Parser a -> Parser [a]
many1 p = liftM2 (:) p (many p)

pConcat :: Parser RegEx
pConcat = foldl1 Concat <$> many1 pTerm

pEpsilon :: Parser RegEx
pEpsilon = do
  _ <- string ""
  return Epsilon

pAlter :: Parser RegEx
pAlter = (pConcat <|> pEpsilon) `chainl1` (lexeme (string "|") >> return Alter)

pRegEx :: Parser RegEx
pRegEx = pAlter

pRange :: Parser RegEx
pRange =
  between (lexeme "[") (lexeme "]") $
    foldr1 Alter . concatMap toLists
      <$> many1
        ( (,)
            <$> satisfy isAlphaNum
            <* lexeme "-"
            <*> satisfy isAlphaNum
        )
  where
    toLists (a, b) = map Literal [a .. b]

pTerm :: Parser RegEx
pTerm = do
  term <-
    choice
      [ pRange,
        pLiteral,
        between (lexeme "(") (lexeme ")") pRegEx
      ]
  s <- optional (many1 (char '*' <|> char '+'))
  return $ case s of
    -- I did a derivation and found (s*)+ = (s+)* = s* so it should hold if *
    -- occurs in a sequence of applied postfix operation then it will equal s*.
    -- If only + occurs in the postfix sequence then then due to (s+)+ = s+ it
    -- will simply correspond to ss*.
    Just postfixes ->
      if any (`elem` ['*']) postfixes
        then Star term
        else Concat term (Star term)
    Nothing -> term

regExFromText :: FilePath -> Text -> Either String RegEx
regExFromText fname s =
  either (Left . errorBundlePretty) Right $ parse (pRegEx <* eof) fname s

data NFA s = NFA
  { states' :: Set s,
    transitions' :: Map (s, Maybe Char) (Set s),
    initial' :: s,
    alphabet' :: Set Char,
    accepting' :: s
  }
  deriving (Show)

initNFA :: (Ord s, Enum s) => s -> NFA s
initNFA start_state =
  NFA
    { states' = Set.fromList [start_state, succ start_state],
      alphabet' = Set.empty,
      transitions' = Map.empty,
      initial' = start_state,
      accepting' = succ start_state
    }

newState :: (Ord s, Enum s) => State (NFA s) s
newState = do
  nfa <- get
  let max_state = Set.findMax $ states' nfa
  let new_max_state = succ max_state
  let new_states' = Set.insert new_max_state $ states' nfa
  put
    ( nfa
        { states' = new_states'
        }
    )
  return new_max_state

newTransition :: (Ord s) => s -> Maybe Char -> s -> State (NFA s) ()
newTransition s c s' = do
  nfa <- get
  let trans = transitions' nfa
  let key = (s, c)
  let new_trans =
        if key `Map.member` trans
          then Map.adjust (Set.insert s') key trans
          else Map.insert key (Set.singleton s') trans
  let new_alph = Set.fromList (maybeToList c) `Set.union` alphabet' nfa
  put $ nfa {transitions' = new_trans, alphabet' = new_alph}

epsilon :: Maybe a
epsilon = Nothing

mkNFA' :: (Ord s, Enum s) => s -> s -> RegEx -> State (NFA s) ()
mkNFA' s s' Epsilon = newTransition s epsilon s'
mkNFA' s s' (Literal c) = newTransition s (Just c) s'
mkNFA' s s'' (Concat a b) = do
  s' <- newState
  mkNFA' s s' a
  mkNFA' s' s'' b
mkNFA' s s'''' (Alter a b) = do
  s' <- newState
  s'' <- newState
  s''' <- newState
  newTransition s epsilon s'
  newTransition s epsilon s''
  mkNFA' s' s''' a
  mkNFA' s'' s''' b
  newTransition s''' epsilon s''''
mkNFA' s s'' (Star a) = do
  s' <- newState
  newTransition s epsilon s'
  newTransition s epsilon s''
  mkNFA' s' s a

mkNFA :: (Show s, Ord s, Enum s) => RegEx -> State (NFA s) ()
mkNFA regex = do
  nfa <- get
  let (s, s') = (initial' nfa, accepting' nfa)
  mkNFA' s s' regex

stateTransitions :: (Show s, Ord s) => Maybe Char -> s -> State (NFA s) (Set s)
stateTransitions c s = do
  nfa <- get
  let trans = transitions' nfa
  let eps_map = Map.filterWithKey (\k _ -> isSymbolTransition k) trans
  return . Set.unions $ toList eps_map
  where
    isSymbolTransition (s', c') = s == s' && c == c'

epsilonTransitions :: (Show s, Ord s) => s -> State (NFA s) (Set s)
epsilonTransitions = stateTransitions Nothing

statesTransitions :: (Show s, Ord s) => Set s -> Maybe Char -> State (NFA s) (Set s)
statesTransitions set c = Set.unions <$> mapM (stateTransitions c) (toList set)

epsilonClosure :: (Show s, Ord s) => Set s -> State (NFA s) (Set s)
epsilonClosure set = do
  new_set <- Set.unions <$> mapM epsilonTransitions (toList set)
  let set' = new_set `Set.union` set
  if set == set'
    then return set'
    else epsilonClosure set'

nfaFromRegEx :: (Show s, Ord s, Enum s) => s -> RegEx -> NFA s
nfaFromRegEx start_state regex = execState (mkNFA regex) init_nfa
  where
    init_nfa = initNFA start_state

mkDFATransitionEntry ::
  (Show s, Ord s, Enum s) =>
  Set s ->
  Char ->
  State (NFA s) (Map (Set s, Char) (Set s))
mkDFATransitionEntry set c = do
  _states <- statesTransitions set $ Just c
  eps_states <- epsilonClosure _states
  return $ Map.singleton (set, c) eps_states

mkDFATransitionEntries ::
  (Show s, Ord s, Enum s) =>
  Set s ->
  State (NFA s) (Map (Set s, Char) (Set s))
mkDFATransitionEntries set = do
  alph <- gets (toList . alphabet')
  new_table_entry <- mapM (mkDFATransitionEntry set) alph
  return $ Map.unionsWith Set.union new_table_entry

mkDFATransitions ::
  (Show s, Ord s, Enum s) =>
  Set (Set s) ->
  Map (Set s, Char) (Set s) ->
  [Set s] ->
  State (NFA s) (Map (Set s, Char) (Set s))
mkDFATransitions _ table [] = return table
mkDFATransitions visited table (top : queue) = do
  entries <- mkDFATransitionEntries top
  let rest = toList $ Map.elems entries
  let new_queue = queue ++ rest
  let new_table = Map.unionWith Set.union entries table
  let new_visited = Set.insert top visited
  if top `Set.member` visited
    then mkDFATransitions visited table queue
    else mkDFATransitions new_visited new_table new_queue

data DFA s = DFA
  { states :: Set s,
    alphabet :: Set Char,
    transitions :: Map (s, Char) s,
    initial :: s,
    accepting :: Set s
  }
  deriving (Eq, Show)

dfaMap :: Ord s => (a -> s) -> DFA a -> DFA s
dfaMap f dfa =
  dfa
    { states = Set.map f (states dfa),
      transitions = f <$> Map.mapKeys (first f) (transitions dfa),
      initial = f $ initial dfa,
      accepting = f `Set.map` accepting dfa
    }

mkDFA :: (Show s, Enum s, Ord s) => RegEx -> State (NFA s) (DFA (Set s))
mkDFA regex = do
  mkNFA regex
  nfa <- get
  let accept = accepting' nfa
  new_initial <- epsilonClosure . Set.singleton $ initial' nfa
  new_transitions <- mkDFATransitions Set.empty Map.empty [new_initial]
  let (new_states, new_alphabet) = bimap Set.fromList Set.fromList . unzip $ Map.keys new_transitions
  let new_accepting = Set.filter (accept `Set.member`) new_states
  return $
    DFA
      { states = new_states,
        alphabet = new_alphabet,
        transitions = new_transitions,
        initial = new_initial,
        accepting = new_accepting
      }

reenumerateDFA :: (Show s, Show s', Ord s, Enum s, Ord s') => s -> DFA s' -> DFA s
reenumerateDFA start_state dfa = dfaMap alphabetMap dfa
  where
    alphabet' = Map.fromList . flip zip [start_state ..] . toList $ states dfa
    alphabetMap = (alphabet' Map.!)

dfaFromRegEx :: (Show s, Ord s, Enum s) => s -> RegEx -> DFA s
dfaFromRegEx start_state regex = reenumerateDFA start_state dfa
  where
    dfa = evalState (mkDFA regex) init_nfa
    init_nfa = initNFA 0 :: NFA Integer

isMatch :: Ord s => DFA s -> Text -> Bool
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