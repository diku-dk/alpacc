module Main where

import MyLib ( Grammar, Terminal, Nonterminal, ExtendedGrammar, extendedGrammarToGrammar )
import qualified Data.ByteString.Lazy as BS
import System.Environment
import Text.ParserCombinators.ReadP
import qualified Data.Char as C

main :: IO ()
main = do
  print . extendedGrammarToGrammar $ (read "( T , e , ( D , { a } , { D } , { D -> a } ) )" :: ExtendedGrammar)
