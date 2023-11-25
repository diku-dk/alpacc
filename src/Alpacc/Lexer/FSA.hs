{-# LANGUAGE MonoLocalBinds #-}
module Alpacc.Lexer.FSA
  ( FSA (..),
    reenumerateFSA,
    reenumerateFSAsMap,
    OrdMap (..),
    IsState,
    IsTransition,
    FSAMappable,
    FSAMap (..),
    Lexer (..),
    LexerMappable,
    LexerMap (..),
    reenumerateLexer
  )
where

import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable
import Control.Monad.Identity (Identity)
import Data.Tuple.Extra (both)
import Data.Word (Word8)

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
    terminalMap :: Map s k
  }
  deriving (Ord, Eq, Show)

class OrdMap f where
  omap :: (Ord a, Ord b) => (a -> b) -> f a -> f b

class (Eq s, Ord s, Show s) => IsState s where

class (Eq t, Ord t, Show t) => IsTransition t where

instance IsTransition Char where

instance IsTransition Word8 where

instance IsTransition Integer where

instance IsTransition Int where

class (IsTransition t, Ord (f' t), OrdMap f, OrdMap f', IsState s) => FSAMappable p f f' t s where

instance (IsTransition t, Ord (f' t), OrdMap f, OrdMap f', IsState s) => FSAMappable FSA f f' t s where

class (IsTransition t, Ord (f' t), OrdMap f, OrdMap f', IsState s, Ord k) => LexerMappable p f f' t s k where

instance (IsTransition t, Ord (f' t), OrdMap f, OrdMap f', IsState s, Ord k) => LexerMappable Lexer f f' t s k where

class FSAMap p where
  fsaMap :: (IsTransition a, IsTransition c, Ord (f' c), OrdMap f, OrdMap f', IsState b, IsState d) => (a -> c) -> (b -> d) -> p f f' a b -> p f f' c d
  fsaFirst :: (IsTransition a, IsTransition c, Ord (f' c), OrdMap f, OrdMap f', IsState b) => (a -> c) -> p f f' a b -> p f f' c b
  fsaFirst = flip fsaMap id
  fsaSecond :: (IsTransition a, Ord (f' a), OrdMap f, OrdMap f', IsState b, IsState d) => (b -> d) -> p f f' a b -> p f f' a d
  fsaSecond = fsaMap id

class LexerMap p where
  fsaLexerMap :: (IsTransition a, IsTransition c, Ord (f' c), OrdMap f, OrdMap f', IsState b, IsState d) => (a -> c) -> (b -> d) -> p f f' a b k -> p f f' c d k
  fsaLexerFirst :: (IsTransition a, IsTransition c, Ord (f' c), OrdMap f, OrdMap f', IsState b) => (a -> c) -> p f f' a b k -> p f f' c b k
  fsaLexerFirst = flip fsaLexerMap id
  fsaLexerSecond :: (IsTransition a, Ord (f' a), OrdMap f, OrdMap f', IsState b, IsState d) => (b -> d) -> p f f' a b k -> p f f' a d k
  fsaLexerSecond = fsaLexerMap id

instance OrdMap Set where
  omap = Set.map

instance IsState s => IsState (Set s) where

instance IsState Integer where

instance IsState Int where

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
        terminalMap = Map.mapKeys f terminal_map
      }
    where
      terminal_map = terminalMap fsa_lexer

reenumerateFSA ::
  (FSAMappable FSA f f' t s', FSAMappable FSA f f' t s, Enum s) =>
  s ->
  FSA f f' t s' ->
  FSA f f' t s
reenumerateFSA start_state fsa = fsaSecond alphabetMap fsa
  where
    alphabet' = Map.fromList . flip zip [start_state ..] . toList $ states fsa
    alphabetMap = (alphabet' Map.!)

reenumerateLexer ::
  (LexerMappable Lexer f f' t s' k, LexerMappable Lexer f f' t s k, Enum s) =>
  s ->
  Lexer f f' t s' k ->
  Lexer f f' t s k
reenumerateLexer start_state fsa_lexer = fsaLexerSecond alphabetMap fsa_lexer
  where
    alphabet' =
      Map.fromList 
      $ flip zip [start_state ..] 
      $ toList
      $ states
      $ fsa fsa_lexer
    alphabetMap = (alphabet' Map.!)

reenumerateFSAsInOrder ::
  (FSAMappable FSA f f' t s', FSAMappable FSA f f' t s, Enum s) =>
  s ->
  [FSA f f' t s'] ->
  [FSA f f' t s]
reenumerateFSAsInOrder _ [] = []
reenumerateFSAsInOrder start_state (fsa:fsas) = scanl f fsa' fsas
  where
    fsa' = reenumerateFSA start_state fsa
    f a = reenumerateFSA (succ . maximum $ states a)

reenumerateFSAsMap ::
  (FSAMappable FSA f f' t s', FSAMappable FSA f f' t s, Ord k, Enum s) =>
  s ->
  Map k (FSA f f' t s') ->
  Map k (FSA f f' t s)
reenumerateFSAsMap start_state =
  Map.fromList
  . uncurry zip
  . second (reenumerateFSAsInOrder start_state)
  . unzip
  . Map.toList
