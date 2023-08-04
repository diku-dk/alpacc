module Alpacc.RegularExpression
  ( regExFromText,
    pRegEx,
    dfaFromRegEx,
    nfaFromRegEx,
    isMatch,
    isMatchPar,
    parallelLexingTable,
    mkTokenizerRegEx,
    DFA (..),
    RegEx (..),
    addDeadStateDFA,
    defaultTransitions
  )
where

import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.Composition
import Data.Foldable (Foldable (..))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Maybe (fromMaybe, maybeToList, fromJust)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Debug.Trace (traceShow)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Data.Tuple.Extra (both)

debug :: Show b => b -> b
debug x = traceShow x x

type Parser = Parsec Void Text

data RegEx t
  = Epsilon
  | Literal Char
  | Range [Either Char (Char, Char)]
  | Star (RegEx t)
  | Alter (RegEx t) (RegEx t)
  | Concat (RegEx t) (RegEx t)
  | Token t (RegEx t)
  deriving (Eq, Show)

space :: Parser ()
space = Lexer.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

validLiterials :: [Char] -- missing chars ";\[]()*|+-"
validLiterials = ['!'..'\''] ++ [','] ++ ['.'..':'] ++ ['<'..'Z'] ++ ['^'..'{'] ++ ['}'..'~']

pBackslash :: Parser Char
pBackslash = Text.last <$> string "\\\\"

pSemicolon :: Parser Char
pSemicolon = Text.last <$> string "\\;"

pLeftSquareBracket :: Parser Char
pLeftSquareBracket = Text.last <$> string "\\["

pRightSquareBracket :: Parser Char
pRightSquareBracket = Text.last <$> string "\\]"

pLeftParentheses :: Parser Char
pLeftParentheses = Text.last <$> string "\\("

pRightParentheses :: Parser Char
pRightParentheses = Text.last <$> string "\\)"

pStar :: Parser Char
pStar = Text.last <$> string "\\*"

pAdd :: Parser Char
pAdd = Text.last <$> string "\\+"

pDash :: Parser Char
pDash = Text.last <$> string "\\-"

pPipe :: Parser Char
pPipe = Text.last <$> string "\\|"

pSpace :: Parser Char
pSpace = do
  _ <- string "\\s"
  return ' '

pTab :: Parser Char
pTab = do
  _ <- string "\\t"
  return '\t'

pNewline :: Parser Char
pNewline = do
  _ <- string "\\n"
  return '\n'

pCarriageReturn :: Parser Char
pCarriageReturn = do
  _ <- string "\\r"
  return '\r'

pChar :: Parser Char
pChar =
  pBackslash <|>
  pSemicolon <|>
  pLeftSquareBracket <|>
  pRightSquareBracket <|>
  pLeftParentheses <|>
  pRightParentheses <|>
  pSpace <|>
  pTab <|>
  pNewline <|>
  pCarriageReturn <|>
  pStar <|>
  pAdd <|>
  pPipe <|>
  pDash <|>
  satisfy (`elem` validLiterials)

pLiteral :: Parser (RegEx t)
pLiteral = Literal <$> lexeme pChar

many1 :: Parser a -> Parser [a]
many1 p = liftM2 (:) p (many p)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x =
      do
        f <- op
        y <- p
        rest (f x y)
        <|> return x

pConcat :: Parser (RegEx t)
pConcat = foldl Concat Epsilon <$> many pTerm

pAlter :: Parser (RegEx t)
pAlter = pConcat `chainl1` (lexeme (string "|") >> return Alter)

pRegEx :: Parser (RegEx t)
pRegEx = pAlter

pRange :: Parser (RegEx t)
pRange =
  between (lexeme "[") (lexeme "]") $
    Range
      <$> many1
        ( Right
            .: (,)
            <$> pChar
            <* lexeme "-"
            <*> pChar
            <|> Left
            <$> pChar
        )

pTerm :: Parser (RegEx t)
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

regExFromText :: FilePath -> Text -> Either String (RegEx t)
regExFromText fname s =
  either (Left . errorBundlePretty) Right $ parse (pRegEx <* eof) fname s

data NFA t s = NFA
  { states' :: Set s,
    transitions' :: Map (s, Maybe Char) (Set s),
    initial' :: s,
    alphabet' :: Set Char,
    accepting' :: Set s,
    terminalMap' :: Map ((s, s), Char) (Set t),
    finalTerminalStates' :: Map t (Set s),
    initialTerminalStates' :: Map t (Set s),
    continueTerminalStates' :: Map t (Set s)
  }
  deriving (Show)

initNFA :: (Ord s, Enum s) => s -> NFA t s
initNFA start_state =
  NFA
    { states' = Set.fromList [start_state, succ start_state],
      alphabet' = Set.empty,
      transitions' = Map.empty,
      initial' = start_state,
      accepting' = Set.singleton $ succ start_state,
      terminalMap' = Map.empty,
      finalTerminalStates' = Map.empty,
      initialTerminalStates' = Map.empty,
      continueTerminalStates' = Map.empty
    }

newState :: (Ord s, Enum s) => State (NFA t s) s
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

newTransition :: (Ord s) => s -> Maybe Char -> s -> State (NFA t s) ()
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

markToken :: (Ord t, Ord s) => (Set ((s, s), Char), Set s) -> t -> s -> s -> State (NFA t s) ()
markToken (set, continue_set') t s s' = do
  nfa <- get

  let continue_sets = continueTerminalStates' nfa
  let continue_set = fromMaybe Set.empty $ Map.lookup t continue_sets
  let new_continue_set = continue_set' `Set.union` continue_set
  let new_continue_sets = Map.insert t new_continue_set continue_sets

  let initial_sets = initialTerminalStates' nfa
  let initial_set = fromMaybe Set.empty $ Map.lookup t initial_sets
  let new_initial_set = Set.insert s initial_set
  let new_initial_sets = Map.insert t new_initial_set initial_sets

  let final_sets = finalTerminalStates' nfa
  let final_set = fromMaybe Set.empty $ Map.lookup t final_sets
  let new_final_set = Set.insert s' final_set
  let new_final_sets = Map.insert t new_final_set final_sets

  let new_map = Map.fromList $ (,Set.singleton t) <$> Set.toList set
  let _map = terminalMap' nfa

  put (nfa {
    terminalMap' = Map.unionWith Set.union new_map _map,
    finalTerminalStates' = new_final_sets,
    initialTerminalStates' = new_initial_sets,
    continueTerminalStates' = new_continue_sets
    })

epsilon :: Maybe a
epsilon = Nothing

combineMkNFAReturn :: Ord s => (Set ((s, s), Char), Set s) -> (Set ((s, s), Char), Set s) -> (Set ((s, s), Char), Set s)
combineMkNFAReturn (a, b) (a', b') = (Set.union a a', Set.union b b')

mkNFA' :: (Ord t, Ord s, Enum s) => s -> s -> RegEx t -> State (NFA t s) (Set ((s, s), Char), Set s)
mkNFA' s s' Epsilon = do
  newTransition s epsilon s'
  return (Set.empty, Set.empty)
mkNFA' s s' (Literal c) = do
  newTransition s (Just c) s'
  return (Set.singleton ((s, s'), c), Set.empty)
mkNFA' s s'' (Concat a b) = do
  s' <- newState
  new <- mkNFA' s s' a
  new' <- mkNFA' s' s'' b
  return $ combineMkNFAReturn new new'
mkNFA' s s'''' (Alter a b) = do
  s' <- newState
  s'' <- newState
  s''' <- newState
  newTransition s epsilon s'
  newTransition s epsilon s''
  newTransition s''' epsilon s''''
  new <- mkNFA' s' s''' a
  new' <- mkNFA' s'' s''' b
  return $ combineMkNFAReturn new new'
mkNFA' s s'' (Star a) = do
  s' <- newState
  newTransition s epsilon s'
  newTransition s epsilon s''
  new <- mkNFA' s' s a
  return $ second (Set.insert s') new
mkNFA' s s''' (Token t a) = do
  s' <- newState
  s'' <- newState
  newTransition s epsilon s'
  newTransition s'' epsilon s'''
  new <- mkNFA' s' s'' a
  markToken new t s' s''
  return new
mkNFA' s s' (Range range) = do
  let chars = concatMap toChars range
  mapM_ (\c -> newTransition s (Just c) s') chars
  return (Set.fromList $ ((s, s'),) <$> chars, Set.empty)
  where
    toChars (Right (a, b)) = [a .. b]
    toChars (Left a) = [a]

mkTokenizerRegEx :: Map t (RegEx t) -> RegEx t
mkTokenizerRegEx regex_map =
  if null regex_map
    then Epsilon
    else Star $ foldr1 Alter $ uncurry Token <$> Map.toList regex_map

mkNFA :: (Ord t, Show s, Ord s, Enum s) => RegEx t -> State (NFA t s) ()
mkNFA regex = do
  nfa <- get
  let (s, s') = (initial' nfa, accepting' nfa)
  let accept_list = toList s'
  mapM_ (\_s -> mkNFA' s _s regex) accept_list

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
  State (NFA t s) (Map (Set s, Char) (Set s))
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

mkDFAFromNFA :: (Show s, Enum s, Ord s, Ord t) => State (NFA t s) (DFA t (Set s))
mkDFAFromNFA = do
  nfa <- get
  let accept = accepting' nfa
  let token_map = terminalMap' nfa
  new_initial <- epsilonClosure . Set.singleton $ initial' nfa
  new_transitions <- mkDFATransitions Set.empty Map.empty [new_initial]
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
  where
    newTokenMap states' = Map.unionsWith Set.union . fmap (newStates states') . Map.toList 
    newStates states' (((s, s'), c), t) =
      Map.fromList [(((a, b), c), t) | a <- newState' s, b <- newState' s']
      where
        newState' _s = toList $ Set.filter (Set.member _s) states'

mkDFAFromRegEx :: (Ord t, Show s, Enum s, Ord s) => RegEx t -> State (NFA t s) (DFA t (Set s))
mkDFAFromRegEx regex = do
  mkNFA regex
  mkDFAFromNFA

reenumerateDFA :: (Show s, Show s', Ord s, Enum s, Ord s') => s -> DFA t s' -> DFA t s
reenumerateDFA start_state dfa = dfaMap alphabetMap dfa
  where
    alphabet' = Map.fromList . flip zip [start_state ..] . toList $ states dfa
    alphabetMap = (alphabet' Map.!)

dfaFromRegEx :: (Ord t, Show s, Ord s, Enum s) => s -> RegEx t -> DFA t s
dfaFromRegEx start_state regex = reenumerateDFA start_state dfa
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
    final_state = debug $ scanl (flip (List.!!)) _initial paths