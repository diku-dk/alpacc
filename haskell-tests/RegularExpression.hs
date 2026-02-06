module RegularExpression (tests) where

import Alpacc.Lexer.DFA
import Alpacc.Lexer.FSA
import Alpacc.Lexer.RegularExpression
import Codec.Binary.UTF8.String (encodeChar)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

textToWord8List :: Text -> [Word8]
textToWord8List = concatMap encodeChar . Text.unpack

regularExpressionMatchCase ::
  (DFA Word8 Integer -> [Word8] -> Bool) ->
  Text ->
  [Text] ->
  [Text] ->
  TestTree
regularExpressionMatchCase isMatch' regex valid invalid =
  testGroup [i|RegEx "#{regex}" test.|] [valid_test, invalid_test]
  where
    valid_test =
      testCase [i|Valid RegEx Strings for "#{regex}" test.|] $
        all (isMatch' dfa . textToWord8List) valid @?= True
    invalid_test =
      testCase [i|Invalid RegEx Strings for "#{regex}" test.|] $
        all (isNotMatch dfa . textToWord8List) invalid @?= True
    Right regex_tree = regExFromText "" regex
    dfa = enumerateFSA 0 $ fromRegExToDFA 0 (fmap unBytes regex_tree)
    isNotMatch dfa' = not . isMatch' dfa'

regularExpressionMatchCase0 :: TestTree
regularExpressionMatchCase0 =
  regularExpressionMatchCase
    isMatch
    "a*|b*"
    ["", "a", "aa", "aaa", "b", "bb", "bbb"]
    ["ab", "ba", "c", "aba", "bba", "xy"]

regularExpressionMatchCase1 :: TestTree
regularExpressionMatchCase1 =
  regularExpressionMatchCase
    isMatch
    "a*b*"
    ["", "ab", "aab", "b", "bb", "aaa", "abb", "aabbb"]
    ["aba", "bab", "xy", "abab", "bbaaa"]

regularExpressionMatchCase2 :: TestTree
regularExpressionMatchCase2 =
  regularExpressionMatchCase
    isMatch
    "a|b|c"
    ["a", "b", "c"]
    ["ab", "abc", "cba", "ac", "bb", "cc", "abcab", "xy", ""]

regularExpressionMatchCase3 :: TestTree
regularExpressionMatchCase3 =
  regularExpressionMatchCase
    isMatch
    "a+b+c+"
    ["abc", "aabbcc", "aaabbbccc", "aaaaaabbbbbbcccccc", "abbc"]
    ["ab", "acb", "bca", "aabccx", "aabcbccc", "abcabc", "xy", ""]

regularExpressionMatchCase4 :: TestTree
regularExpressionMatchCase4 =
  regularExpressionMatchCase
    isMatch
    "[a-z]+"
    ["abc", "aabbcc", "aaabbbccc", "aaaaaabbbbbbcccccc", "abbc", "ab", "acb", "bca", "aabccx", "aabcbccc", "abcabc", "xy"]
    ["", "4234", "324", "1", "<", "#", "\\"]

regularExpressionMatchCase5 :: TestTree
regularExpressionMatchCase5 =
  regularExpressionMatchCase
    isMatch
    "(a*b|c)d+(e|)"
    ["bd", "bde", "abd", "cd", "abdde", "cdde", "abde"]
    ["a", "b", "aa", "bb", "cc", "ade", "de", "xbdde", "abbddee"]

regularExpressionMatchCase6 :: TestTree
regularExpressionMatchCase6 =
  regularExpressionMatchCase
    isMatch
    "(xy*|z)ab+(c|)"
    ["xab", "xyab", "xyyab", "zab", "xabc", "xyabc", "xyyyabc"]
    ["x", "xy", "xz", "ab", "abb", "xyyy", "abc4", "zatbc"]

regularExpressionMatchCase7 :: TestTree
regularExpressionMatchCase7 =
  regularExpressionMatchCase
    isMatch
    "a*b+c*(d|)e+f"
    ["aabcdeef", "aabbcdeef", "abbcdef", "abbcccdeef", "abdeef"]
    ["aaaeef", "abcdf", "aabbcczdeef", "aabccddeef", "abccddef", "xy", ""]

regularExpressionMatchCase8 :: TestTree
regularExpressionMatchCase8 =
  regularExpressionMatchCase
    isMatch
    "(cat|dog|fish)+([0-9]|)"
    ["catfish", "catcatfish", "dogdogdog4", "fish3", "catdogfish0", "cat3"]
    ["catdoga", "0fishfish", "4cat4", " dogdogdogdog", "ddogfish3", "xy", ""]

regularExpressionMatchCase9 :: TestTree
regularExpressionMatchCase9 =
  regularExpressionMatchCase
    isMatch
    "()"
    [""]
    ["catdoga", "0fishfish", "4cat4", " dogdogdogdog", "ddogfish3", "xy"]

regularExpressionMatchCase10 :: TestTree
regularExpressionMatchCase10 =
  regularExpressionMatchCase
    isMatch
    ""
    [""]
    ["catdoga", "0fishfish", "4cat4", " dogdogdogdog", "ddogfish3", "xy"]

tests =
  testGroup "Regular Expression tests" $
    [ regularExpressionMatchCase0,
      regularExpressionMatchCase1,
      regularExpressionMatchCase2,
      regularExpressionMatchCase3,
      regularExpressionMatchCase4,
      regularExpressionMatchCase5,
      regularExpressionMatchCase6,
      regularExpressionMatchCase7,
      regularExpressionMatchCase8,
      regularExpressionMatchCase9,
      regularExpressionMatchCase10
    ]
