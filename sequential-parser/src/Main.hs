module Main where

import qualified Data.Map as M
import GHC.Generics ( Generic )
import Data.Aeson ( decode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as S
import Data.Maybe ( fromJust )
import Debug.Trace ( traceShow )
import qualified Data.List as L

data GrammarData =
  Grammar
    { start               :: String
    , extendStart         :: String
    , extendEnd           :: String
    , terminals           :: [String]
    , nonterminals        :: [String]
    , grammar             :: M.Map String [[String]]
    } deriving (Generic, Show)

instance FromJSON GrammarData
instance ToJSON GrammarData

data Grammar a = Nonterminal a | Terminal a deriving (Eq, Show, Ord)

toGrammar :: GrammarData -> M.Map (Grammar String) [[Grammar String]]
toGrammar d = M.mapKeys Nonterminal . fmap (fmap (fmap toGrammar)) $ grammar d
  where
    toGrammar a
      | a `elem` terminals d = Terminal a
      | a `elem` nonterminals d = Nonterminal a
      | otherwise = error $ "'" ++ show a ++ "' is neither a terminal or a nonterminal."

nullable :: M.Map (Grammar String) [[Grammar String]] -> [Grammar String] -> Bool
nullable grammar = auxiliary S.empty
  where
    auxiliary _ [] = True
    auxiliary _ [Terminal _] = False
    auxiliary visited [nonterminal]
      | nonterminal `S.member` visited = False
      | otherwise = any (auxiliary new_visited) $ grammar M.! nonterminal
      where
        new_visited = S.insert nonterminal visited
    auxiliary visited as = all (auxiliary visited . L.singleton) as

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore _ [] = []
takeWhileOneMore predicate (x:xs)
  | predicate x = x : takeWhileOneMore predicate xs
  | otherwise = [x]

first :: M.Map (Grammar String) [[Grammar String]] -> [Grammar String] -> S.Set (Grammar String)
first grammar = auxiliary S.empty
  where
    auxiliary _ [] = S.empty
    auxiliary _ [Terminal a] = S.singleton $ Terminal a
    auxiliary visited [nonterminal]
      | nonterminal `S.member` visited = S.empty
      | otherwise = S.unions $ auxiliary new_visited <$> grammar M.! nonterminal
      where
        new_visited = S.insert nonterminal visited
    auxiliary visited as = S.unions $ auxiliary visited . L.singleton <$> takeWhileOneMore (nullable grammar . L.singleton) as

rightSymbols :: [Grammar a] -> [(Grammar a, [Grammar a])]
rightSymbols [] = []
rightSymbols ((Terminal _):xs) = rightSymbols xs
rightSymbols (x:xs) = (x, xs) : rightSymbols xs

data FollowContraint a = SetConstraint (S.Set (Grammar a)) (Grammar a)
                       | GraConstraint (Grammar a) (Grammar a) deriving (Eq, Show, Ord)

contraint :: M.Map (Grammar String) [[Grammar String]] -> Grammar String -> [Grammar String] -> S.Set (FollowContraint String)
contraint grammar nonterminal seq = S.unions $ uncurry auxiliary <$> right_symbols
  where
    right_symbols = rightSymbols seq
    auxiliary nonterminal' right = fstCond `S.union` sndCond
      where
        set = first grammar right
        fstCond = if S.null set
                  then S.empty
                  else S.singleton (SetConstraint set nonterminal')
        sndCond = if nonterminal /= nonterminal' && nullable grammar right
                  then S.singleton (GraConstraint nonterminal nonterminal')
                  else S.empty

constraints :: M.Map (Grammar String) [[Grammar String]] -> S.Set (FollowContraint String)
constraints grammar = S.unions $ M.mapWithKey (\k v -> S.unions $ contraint grammar k <$> v) grammar

toExtendedGrammar :: GrammarData -> M.Map (Grammar String) [[Grammar String]]
toExtendedGrammar grammar_data = toGrammar new_grammar_data
  where
    new_start = extendStart grammar_data
    old_start = start grammar_data
    end = extendEnd grammar_data
    new_terminals = end : terminals grammar_data
    new_nonterminals = new_start : nonterminals grammar_data
    new_grammar = M.insert new_start [[old_start, end]] (grammar grammar_data)
    new_grammar_data = grammar_data { grammar = new_grammar
                                    , start = new_start
                                    , terminals = new_terminals
                                    , nonterminals = new_nonterminals}

follow :: Ord a => S.Set (FollowContraint a) -> M.Map (Grammar a) (S.Set (Grammar a))
follow constraints' = fst . head $ dropWhile (uncurry (/=)) iterations
  where
    iterations = iterate auxiliary (empty_follow_sets, M.empty)
    auxiliary (a, b) = (foldl (flip addConstraint) a constraints', a)
    nonterminals' [] = []
    nonterminals' ((SetConstraint _ a):xs) = a : nonterminals' xs
    nonterminals' ((GraConstraint a b):xs) = a : b : nonterminals' xs
    empty_follow_sets = M.fromList . map (,S.empty) . nonterminals' $ S.toList constraints'
    addConstraint (SetConstraint set a) m = M.adjust (`S.union` set) a m
    addConstraint (GraConstraint a b) m = M.adjust (`S.union` (m M.! a)) b m

main :: IO ()
main = do
  input <- BS.getContents
  let grammar_data = fromJust (decode input :: Maybe GrammarData)
  let grammar = toGrammar grammar_data
  let extended_grammar = toExtendedGrammar grammar_data
  mapM_ print $ M.mapWithKey (\k v -> (k, nullable grammar <$> v)) grammar
  putStrLn ""
  mapM_ print $ M.mapWithKey (\k v -> (k, first grammar <$> v)) grammar
  putStrLn ""
  mapM_ print $ constraints extended_grammar
  putStrLn ""
  mapM_ print . M.toList . follow $ constraints extended_grammar
