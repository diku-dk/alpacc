{-# LANGUAGE MonoLocalBinds #-}
module Alpacc.Lexer.FSA
  ( FSA (..),
    enumerateFSA,
    enumerateFSAsMap,
    OrdMap (..),
    FSAMappable,
    FSAMap (..),
    Lexer (..),
    LexerMappable,
    LexerMap (..),
    enumerateLexer
  )
where

import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable
import Control.Monad.Identity (Identity)

data FSA f f' t s = FSA
  { states :: Set s,
    alphabet :: Set t,
    transitions :: Map (s, f' t) (f s),
    initial :: s,
    accepting :: Set s
  }
  deriving (Ord, Eq, Show)

data Lexer f f' t s k = Lexer
  { fsa :: FSA f f' t s,
    tokenMap :: Map s k,
    producesToken :: Set (s, t)
  }
  deriving (Ord, Eq, Show)

class OrdMap f where
  omap :: (Ord a, Ord b) => (a -> b) -> f a -> f b

class (Ord t, Ord (f' t), OrdMap f, OrdMap f', Ord s) => FSAMappable p f f' t s where

instance (Ord t, Ord (f' t), OrdMap f, OrdMap f', Ord s) => FSAMappable FSA f f' t s where

class (Ord t, Ord (f' t), OrdMap f, OrdMap f', Ord s, Ord k) => LexerMappable p f f' t s k where

instance (Ord t, Ord (f' t), OrdMap f, OrdMap f', Ord s, Ord k) => LexerMappable Lexer f f' t s k where

class FSAMap p where
  fsaMap :: (Ord a, Ord c, Ord (f' c), OrdMap f, OrdMap f', Ord b, Ord d) => (a -> c) -> (b -> d) -> p f f' a b -> p f f' c d
  fsaFirst :: (Ord a, Ord c, Ord (f' c), OrdMap f, OrdMap f', Ord b) => (a -> c) -> p f f' a b -> p f f' c b
  fsaFirst = flip fsaMap id
  fsaSecond :: (Ord a, Ord (f' a), OrdMap f, OrdMap f', Ord b, Ord d) => (b -> d) -> p f f' a b -> p f f' a d
  fsaSecond = fsaMap id

class LexerMap p where
  fsaLexerMap :: (Ord a, Ord c, Ord (f' c), OrdMap f, OrdMap f', Ord b, Ord d) => (a -> c) -> (b -> d) -> p f f' a b k -> p f f' c d k
  fsaLexerFirst :: (Ord a, Ord c, Ord (f' c), OrdMap f, OrdMap f', Ord b) => (a -> c) -> p f f' a b k -> p f f' c b k
  fsaLexerFirst = flip fsaLexerMap id
  fsaLexerSecond :: (Ord a, Ord (f' a), OrdMap f, OrdMap f', Ord b, Ord d) => (b -> d) -> p f f' a b k -> p f f' a d k
  fsaLexerSecond = fsaLexerMap id

instance OrdMap Set where
  omap = Set.map

instance OrdMap Identity where
  omap = fmap

instance FSAMap FSA where
  fsaMap g f fsa =
    fsa
      { states = Set.map f (states fsa),
        alphabet = Set.map g (alphabet fsa),
        transitions = omap f <$> Map.mapKeys (bimap f (omap g)) (transitions fsa),
        initial = f $ initial fsa,
        accepting = f `Set.map` accepting fsa
      }

instance LexerMap Lexer where
  fsaLexerMap g f fsa_lexer =
    fsa_lexer
      { fsa = fsaMap g f $ fsa fsa_lexer,
        tokenMap = Map.mapKeys f token_map,
        producesToken = Set.map (bimap f g) produces_token 
      }
    where
      produces_token = producesToken fsa_lexer
      token_map = tokenMap fsa_lexer

enumerateFSA ::
  (FSAMappable FSA f f' t s', FSAMappable FSA f f' t s, Enum s) =>
  s ->
  FSA f f' t s' ->
  FSA f f' t s
enumerateFSA start_state fsa = fsaSecond alphabetMap fsa
  where
    alphabet' = Map.fromList . flip zip [start_state ..] . toList $ states fsa
    alphabetMap = (alphabet' Map.!)

enumerateLexer ::
  (LexerMappable Lexer f f' t s' k, LexerMappable Lexer f f' t s k, Enum s) =>
  s ->
  Lexer f f' t s' k ->
  Lexer f f' t s k
enumerateLexer start_state fsa_lexer = fsaLexerSecond alphabetMap fsa_lexer
  where
    alphabet' =
      Map.fromList 
      $ flip zip [start_state ..] 
      $ toList
      $ states
      $ fsa fsa_lexer
    alphabetMap = (alphabet' Map.!)

enumerateFSAsInOrder ::
  (FSAMappable FSA f f' t s', FSAMappable FSA f f' t s, Enum s) =>
  s ->
  [FSA f f' t s'] ->
  [FSA f f' t s]
enumerateFSAsInOrder _ [] = []
enumerateFSAsInOrder start_state (fsa:fsas) = scanl f fsa' fsas
  where
    fsa' = enumerateFSA start_state fsa
    f a = enumerateFSA (succ . maximum $ states a)

enumerateFSAsMap ::
  (FSAMappable FSA f f' t s', FSAMappable FSA f f' t s, Ord k, Enum s) =>
  s ->
  Map k (FSA f f' t s') ->
  Map k (FSA f f' t s)
enumerateFSAsMap start_state =
  Map.fromList
  . uncurry zip
  . second (enumerateFSAsInOrder start_state)
  . unzip
  . Map.toList
