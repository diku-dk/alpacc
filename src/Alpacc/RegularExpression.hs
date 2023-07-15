module Alpacc.RegularExpression (regExFromText, pRegEx, nfaFromRegEx, NFA) where

import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Text (Text)
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
pLiteral = Literal <$> lexeme (satisfy (`elem` ['a' .. 'z']))

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

pAlter :: Parser RegEx
pAlter = pConcat `chainl1` (lexeme (string "|") >> return Alter)

pRegEx :: Parser RegEx
pRegEx = pAlter

pTerm :: Parser RegEx
pTerm = do
  term <-
    choice
      [ pLiteral,
        between (lexeme "(") (lexeme ")") pRegEx
      ]
  s <- many (char '*')
  return $ if null s then term else Star term

regExFromText :: FilePath -> Text -> Either String RegEx
regExFromText fname s =
  either (Left . errorBundlePretty) Right $ parse (pRegEx <* eof) fname s

data NFAContext s = NFAContext
  { states :: Set s,
    transitions :: Map (s, Maybe Char) (Set s),
    initial :: s,
    accepting :: s,
    stateMap :: Map s s
  }
  deriving (Show)

initNFAContext :: (Ord s, Enum s, Bounded s) => NFAContext s
initNFAContext =
  NFAContext
    { states = Set.fromList [minBound, succ minBound],
      transitions = Map.empty,
      initial = minBound,
      accepting = succ minBound,
      stateMap = Map.fromList [(minBound, minBound), (succ minBound, succ minBound)]
    }

newState :: (Ord s, Enum s) => State (NFAContext s) s
newState = do
  nfa <- get
  let max_state = Set.findMax $ states nfa
  let new_max_state = succ max_state
  let new_states = Set.insert new_max_state $ states nfa
  let state_map = stateMap nfa
  let new_state_map = Map.insert new_max_state new_max_state state_map
  put (nfa {states = new_states, stateMap = new_state_map})
  return new_max_state

upsert :: (Ord s) => s -> Maybe Char -> s -> State (NFAContext s) ()
upsert s c s' = do
  nfa <- get
  let trans = transitions nfa
  let state_map = stateMap nfa
  let key = (state_map Map.! s, c)
  let new_trans =
        if key `Map.member` trans
          then Map.adjust (Set.insert s') key trans
          else Map.insert key (Set.singleton s') trans
  put $ nfa {transitions = new_trans}

setEqual :: (Ord s) => s -> s -> State (NFAContext s) ()
setEqual s s' = do
  nfa <- get
  let state_map = stateMap nfa
  let new_state_map = Map.insert s' s state_map
  let replace s'' = if s'' == s' then s else s''
  let new_transitions =
        fmap (Set.map replace)
          . Map.mapKeys (first replace)
          $ transitions nfa
  put
    ( nfa
        { transitions = new_transitions,
          stateMap = new_state_map,
          initial = replace $ initial nfa,
          accepting = replace $ accepting nfa
        }
    )

epsilon :: Maybe a
epsilon = Nothing

mkNFA' :: (Ord s, Enum s) => s -> s -> RegEx -> State (NFAContext s) ()
mkNFA' s s' Epsilon = upsert s epsilon s'
mkNFA' s s' (Literal c) = upsert s (Just c) s'
mkNFA' s s'' (Concat a b) = do
  s' <- newState
  mkNFA' s s' a
  mkNFA' s' s'' b
mkNFA' s s' (Alter a b) = do
  mkNFA' s s' a
  mkNFA' s s' b
mkNFA' s s' (Star a) = do
  mkNFA' s s' a
  setEqual s s'

nfaFromRegEx :: (Ord s, Enum s, Bounded s) => RegEx -> NFAContext s
nfaFromRegEx regex = execState (mkNFA' s s' regex) init_nfa
  where
    init_nfa = initNFA
    (s, s') = (initial init_nfa, accepting init_nfa)