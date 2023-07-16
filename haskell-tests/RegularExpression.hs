module RegularExpression (tests) where

import Alpacc.RegularExpression
import Data.String.Interpolate (i)
import Test.HUnit

regularExpressionCase regex valid invalid =
  TestLabel [i|RegEx "#{regex}" test.|] $
    TestList [valid_test, invalid_test]
  where
    valid_test =
      TestCase $
        assertBool [i|Valid RegEx Strings for "#{regex}" test.|] $
          all (isMatch dfa) valid
    invalid_test =
      TestCase
        $ assertBool
          [i|Invalid RegEx Strings for "#{regex}" test.|]
        $ all (isNotMatch dfa) invalid
    Right regex_tree = regExFromText "" regex
    dfa = dfaFromRegEx 0 regex_tree
    isNotMatch dfa' = not . isMatch dfa'

regularExpressionCase0 =
  regularExpressionCase
    "a*|b*"
    ["", "a", "aa", "aaa", "b", "bb", "bbb"]
    ["ab", "ba", "c", "aba", "bba", "xy"]

regularExpressionCase1 =
  regularExpressionCase
    "a*b*"
    ["", "ab", "aab", "b", "bb", "aaa", "abb", "aabbb"]
    ["aba", "bab", "xy", "abab", "bbaaa"]

regularExpressionCase2 =
  regularExpressionCase
    "a|b|c"
    ["a", "b", "c"]
    ["ab", "abc", "cba", "ac", "bb", "cc", "abcab", "xy", ""]

regularExpressionCase3 =
  regularExpressionCase
    "a+b+c+"
    ["abc", "aabbcc", "aaabbbccc", "aaaaaabbbbbbcccccc", "abbc"]
    ["ab", "acb", "bca", "aabccx", "aabcbccc", "abcabc", "xy", ""]

regularExpressionCase4 =
  regularExpressionCase
    "[a-z]+"
    ["abc", "aabbcc", "aaabbbccc", "aaaaaabbbbbbcccccc", "abbc", "ab", "acb", "bca", "aabccx", "aabcbccc", "abcabc", "xy"]
    ["", "4234", "324", "1", "<", "#", "\\"]

regularExpressionCase5 =
  regularExpressionCase
    "(a*b|c)d+(e|)"
    ["bd", "bde", "abd", "cd", "abdde", "cdde", "abde"]
    ["a", "b", "aa", "bb", "cc", "ade", "de", "xbdde", "abbddee"]

regularExpressionCase6 =
  regularExpressionCase
    "(xy*|z)ab+(c|)"
    ["xab", "xyab", "xyyab", "zab", "xabc", "xyabc", "xyyyabc"]
    ["x", "xy", "xz", "ab", "abb", "xyyy", "abc4", "zatbc"]

regularExpressionCase7 =
  regularExpressionCase
    "a*b+c*(d|)e+f"
    ["aabcdeef", "aabbcdeef", "abbcdef", "abbcccdeef", "abdeef"]
    ["aaaeef", "abcdf", "aabbcczdeef", "aabccddeef", "abccddef", "xy", ""]

regularExpressionCase8 =
  regularExpressionCase
    "(cat|dog|fish)+([0-9]|)"
    ["catfish", "catcatfish", "dogdogdog4", "fish3", "catdogfish0", "cat3"]
    ["catdoga", "0fishfish", "4cat4", " dogdogdogdog", "ddogfish3", "xy", ""]

tests =
  TestLabel "Regular Expression tests" $
    TestList
      [ regularExpressionCase0,
        regularExpressionCase1,
        regularExpressionCase2,
        regularExpressionCase3,
        regularExpressionCase4,
        regularExpressionCase5,
        regularExpressionCase6,
        regularExpressionCase7,
        regularExpressionCase8
      ]
