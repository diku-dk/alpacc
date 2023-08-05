module Alpacc.Generator.Futhark.Generator
  ( generate )
where

import Data.String.Interpolate (i)
import Alpacc.Generator.Futhark.Lexer
import Alpacc.Generator.Futhark.Parser
import Alpacc.Grammar
import Alpacc.RegularExpression


generate :: Int -> Int -> Grammar NT T -> RegEx T -> Either String String
generate q k grammar regex = do
  parser <- generateParser q k grammar
  lexer <- generateLexer grammar regex
  return $ 
    parser <>
    lexer <>
    [i|
entry parse s =
  match lexer.lexer s
  case #just s' -> parser.parse s'.0
  case #nothing -> []
|]