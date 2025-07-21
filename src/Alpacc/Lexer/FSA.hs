module Alpacc.Lexer.FSA
  ( FSA (..),
    enumerateFSA,
    enumerateFSAsMap,
    OrdMap (..),
    Lexer (..),
    fsaMap,
    fsaFirst,
    fsaSecond,
    fsaLexerMap,
    fsaLexerFirst,
    fsaLexerSecond,
    enumerateLexer,
    curryTransitions,
  )
where

import Control.Monad.Identity (Identity)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)

data FSA f t s = FSA
  { states :: Set s,
    alphabet :: Set t,
    transitions :: Map (s, t) (f s),
    initial :: s,
    accepting :: Set s
  }
  deriving (Ord, Eq, Show)

data Lexer f t s k = Lexer
  { fsa :: FSA f t s,
    tokenMap :: Map s k,
    producesToken :: Set (s, t)
  }
  deriving (Ord, Eq, Show)

class OrdMap f where
  omap :: (Ord a, Ord b) => (a -> b) -> f a -> f b

fsaMap :: (Ord a, Ord c, OrdMap f, Ord b, Ord d) => (a -> c) -> (b -> d) -> FSA f a b -> FSA f c d
fsaMap g f fsa =
  fsa
    { states = Set.map f (states fsa),
      alphabet = Set.map g (alphabet fsa),
      transitions = omap f <$> Map.mapKeys (bimap f g) (transitions fsa),
      initial = f $ initial fsa,
      accepting = f `Set.map` accepting fsa
    }

fsaFirst :: (Ord a, Ord c, OrdMap f, Ord b) => (a -> c) -> FSA f a b -> FSA f c b
fsaFirst = flip fsaMap id

fsaSecond :: (Ord a, OrdMap f, Ord b, Ord d) => (b -> d) -> FSA f a b -> FSA f a d
fsaSecond = fsaMap id

fsaLexerMap :: (Ord a, Ord c, OrdMap f, Ord b, Ord d) => (a -> c) -> (b -> d) -> Lexer f a b k -> Lexer f c d k
fsaLexerMap g f fsa_lexer =
  fsa_lexer
    { fsa = fsaMap g f $ fsa fsa_lexer,
      tokenMap = Map.mapKeys f token_map,
      producesToken = Set.map (bimap f g) produces_token
    }
  where
    produces_token = producesToken fsa_lexer
    token_map = tokenMap fsa_lexer

fsaLexerFirst :: (Ord a, Ord c, OrdMap f, Ord b) => (a -> c) -> Lexer f a b k -> Lexer f c b k
fsaLexerFirst = flip fsaLexerMap id

fsaLexerSecond :: (Ord a, OrdMap f, Ord b, Ord d) => (b -> d) -> Lexer f a b k -> Lexer f a d k
fsaLexerSecond = fsaLexerMap id

instance OrdMap Set where
  omap = Set.map

instance OrdMap Identity where
  omap = fmap

enumerateFSA ::
  (Ord t, OrdMap f, Ord s, Ord s', Enum s) =>
  s ->
  FSA f t s' ->
  FSA f t s
enumerateFSA start_state fsa = fsaSecond alphabetMap fsa
  where
    alphabet' = Map.fromList . flip zip [start_state ..] . toList $ states fsa
    alphabetMap = (alphabet' Map.!)

enumerateLexer ::
  (Ord t, OrdMap f, Ord s, Ord s', Enum s) =>
  s ->
  Lexer f t s' k ->
  Lexer f t s k
enumerateLexer start_state fsa_lexer = fsaLexerSecond alphabetMap fsa_lexer
  where
    alphabet' =
      Map.fromList $
        flip zip [start_state ..] $
          toList $
            states $
              fsa fsa_lexer
    alphabetMap = (alphabet' Map.!)

enumerateFSAsInOrder ::
  (Ord t, OrdMap f, Ord s, Ord s', Enum s) =>
  s ->
  [FSA f t s'] ->
  [FSA f t s]
enumerateFSAsInOrder _ [] = []
enumerateFSAsInOrder start_state (fsa : fsas) = scanl f fsa' fsas
  where
    fsa' = enumerateFSA start_state fsa
    f a = enumerateFSA (succ . maximum $ states a)

enumerateFSAsMap ::
  (Ord t, OrdMap f, Ord s, Ord s', Enum s, Ord k) =>
  s ->
  Map k (FSA f t s') ->
  Map k (FSA f t s)
enumerateFSAsMap start_state =
  Map.fromList
    . uncurry zip
    . second (enumerateFSAsInOrder start_state)
    . unzip
    . Map.toList

curryTransitions :: (Ord s, Ord t) => Map (s, t) (f s) -> Map s (Map t (f s))
curryTransitions =
  Map.unionsWith Map.union
    . fmap toMap
    . Map.toList
  where
    toMap ((s, t), s') = Map.singleton s $ Map.singleton t s'
