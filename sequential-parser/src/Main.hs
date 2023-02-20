module Main where

import System.Environment
import qualified Data.Map as M
import GHC.Generics ( Generic )
import Data.Aeson ( decode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace ( traceShow )
import qualified Data.List as L
import Data.Bifunctor (bimap)

debug x = traceShow ("DEBUG: " ++ show x) x

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

data Grammar nt t = Nonterminal nt | Terminal t deriving (Eq, Show, Ord)

toGrammar :: GrammarData -> M.Map String [[Grammar String String]]
toGrammar d = fmap (fmap (fmap toGrammar)) $ grammar d
  where
    toGrammar a
      | a `elem` terminals d = Terminal a
      | a `elem` nonterminals d = Nonterminal a
      | otherwise = error $ "'" ++ show a ++ "' is neither a terminal or a nonterminal."

nullable :: Ord nt => M.Map nt [[Grammar nt t]] -> [Grammar nt t] -> Bool
nullable grammar = auxiliary S.empty
  where
    auxiliary1 _ (Terminal _) = False
    auxiliary1 visited (Nonterminal nonterminal)
      | nonterminal `S.member` visited = False
      | otherwise = any (auxiliary new_visited) $ grammar M.! nonterminal
      where
        new_visited = S.insert nonterminal visited
    auxiliary visited as = all (auxiliary1 visited) as

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore _ [] = []
takeWhileOneMore predicate (x:xs)
  | predicate x = x : takeWhileOneMore predicate xs
  | otherwise = [x]

first :: M.Map String [[Grammar String String]] -> [Grammar String String] -> S.Set String
first grammar = auxiliary S.empty
  where
    auxiliary _ [] = S.empty
    auxiliary _ [Terminal a] = S.singleton a
    auxiliary visited [Nonterminal nonterminal]
      | nonterminal `S.member` visited = S.empty
      | otherwise = S.unions $ auxiliary new_visited <$> grammar M.! nonterminal
      where
        new_visited = S.insert nonterminal visited
    auxiliary visited as = S.unions $ auxiliary visited . L.singleton <$> nullables
      where nullables = takeWhileOneMore (nullable grammar . L.singleton) as

rightSymbols :: [Grammar nt t] -> [(nt, [Grammar nt t])]
rightSymbols [] = []
rightSymbols ((Terminal _):xs) = rightSymbols xs
rightSymbols ((Nonterminal x):xs) = (x, xs) : rightSymbols xs

data FollowContraint nt t = SetConstraint (S.Set t) nt
                          | GraConstraint nt nt deriving (Eq, Show, Ord)

contraint :: M.Map String [[Grammar String String]] -> String -> [Grammar String String] -> S.Set (FollowContraint String String)
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

constraints :: M.Map String [[Grammar String String]] -> S.Set (FollowContraint String String)
constraints grammar = S.unions $ M.mapWithKey (\k v -> S.unions $ contraint grammar k <$> v) grammar

toExtendedGrammar :: GrammarData -> GrammarData
toExtendedGrammar grammar_data = new_grammar_data
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

follow :: M.Map String [[Grammar String String]] -> M.Map String (S.Set String)
follow grammar' = fst . head $ dropWhile (uncurry (/=)) iterations
  where
    constraints' = constraints grammar'
    iterations = iterate auxiliary (empty_follow_sets, M.empty)
    auxiliary (a, b) = (foldl (flip addConstraint) a constraints', a)
    nonterminals' [] = []
    nonterminals' ((SetConstraint _ a):xs) = a : nonterminals' xs
    nonterminals' ((GraConstraint a b):xs) = a : b : nonterminals' xs
    empty_follow_sets = M.fromList . map (,S.empty) . nonterminals' $ S.toList constraints'
    addConstraint (SetConstraint set a) m = M.adjust (`S.union` set) a m
    addConstraint (GraConstraint a b) m = M.adjust (`S.union` (m M.! a)) b m

firstTable :: M.Map String [[Grammar String String]] -> M.Map (String, String) [Grammar String String]
firstTable extended_grammar = M.fromList firsts
  where
    firsts = concatMap auxiliary $ concatMap aux $ M.toList extended_grammar
    aux (a, as) = (a,) <$> as 
    auxiliary (a, as) = [((a, y), as) | y <- S.toList $ first extended_grammar as]

nullableFollowTable :: M.Map String [[Grammar String String]] -> M.Map (String, String) [Grammar String String]
nullableFollowTable extended_grammar = toMap $ rearrangeTuples <$> intersections
  where
    toMap = M.fromList . S.toList . S.unions
    rearrangeTuples (non, (ters, prod)) = (\ter -> ((non, ter), prod)) `S.map` ters
    intersections = M.toList $ M.intersectionWith (,) followTable nullableTable
    followTable = follow extended_grammar
    -- Does not make sure the production rule i unique it just takes the first
    first_prod = fmap head . M.filter (not . L.null)
    nullableTable = first_prod $ L.filter (nullable extended_grammar) <$> extended_grammar

unpack :: Grammar String String -> String
unpack (Nonterminal a) = a
unpack (Terminal a) = a

mkTable :: GrammarData -> M.Map (String, String) [String]
mkTable grammar_data = fmap unpack <$> (first_table `M.union` nullable_follow_table)
  where
    extended_grammar = toGrammar $ toExtendedGrammar grammar_data
    nullable_follow_table = nullableFollowTable extended_grammar
    first_table = firstTable extended_grammar

parse :: GrammarData -> [String] -> [(String, [String])]
parse grammar_data str = auxiliary str [extendStart grammar_data]
  where
    table = mkTable grammar_data
    auxiliary [] [] = []
    auxiliary _ [] = []
    auxiliary [] _ = []
    auxiliary (y:input) (x:stack)
      | y == x = auxiliary input stack
      | otherwise = ((x, production) :) $ auxiliary (y:input) (production ++ stack)
      where
        production = table M.! (x, y)

main :: IO ()
main = do
  args <- getArgs
  input <- BS.readFile $ head args
  print input
  let grammar_data = fromJust (decode input :: Maybe GrammarData)
  let grammar = toGrammar grammar_data
  let extended_grammar = toGrammar $ toExtendedGrammar grammar_data
  putStrLn "Nullable Productions:"
  mapM_ print $ M.mapWithKey (\k v -> (k, nullable grammar <$> v)) grammar
  putStrLn "First sets of Productions:"
  mapM_ print $ M.mapWithKey (\k v -> (k, first grammar <$> v)) grammar
  putStrLn "Constraints of Grammar:"
  mapM_ print $ constraints extended_grammar
  putStrLn "Follow sets of productions"
  mapM_ print . M.toList $  follow extended_grammar
  putStrLn "Table where a in FIRST(alpha)"
  mapM_ print $ firstTable grammar
  putStrLn "Table where a in FOLLOW(a) and NULLABLE(alpha)"
  mapM_ print . M.toList $ nullableFollowTable extended_grammar
  putStrLn "The table for tablen driven LL(1) parsing."
  mapM_ print . M.toList $ mkTable grammar_data
  putStrLn ""
  putStrLn "Input that will be parsed:"
  parse_this <- getLine
  mapM_ print . parse grammar_data $ L.singleton <$> parse_this