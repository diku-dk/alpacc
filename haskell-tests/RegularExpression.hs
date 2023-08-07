module RegularExpression (tests) where

import Alpacc.Lexer.RegularExpression
import Alpacc.Lexer.DFA
import Data.String.Interpolate (i)
import Test.HUnit

regularExpressionMatchCase isMatch' regex valid invalid =
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
    Right dfa = dfaFromRegEx 0 regex_tree
    isNotMatch dfa' = not . isMatch' dfa'

regularExpressionMatchCase0 =
  regularExpressionMatchCase
    isMatch
    "a*|b*"
    ["", "a", "aa", "aaa", "b", "bb", "bbb"]
    ["ab", "ba", "c", "aba", "bba", "xy"]

regularExpressionMatchParCase0 =
  regularExpressionMatchCase
    isMatchPar
    "a*|b*"
    ["", "a", "aa", "aaa", "b", "bb", "bbb"]
    ["ab", "ba", "c", "aba", "bba", "xy"]

regularExpressionMatchCase1 =
  regularExpressionMatchCase
    isMatch
    "a*b*"
    ["", "ab", "aab", "b", "bb", "aaa", "abb", "aabbb"]
    ["aba", "bab", "xy", "abab", "bbaaa"]

regularExpressionMatchParCase1 =
  regularExpressionMatchCase
    isMatchPar
    "a*b*"
    ["", "ab", "aab", "b", "bb", "aaa", "abb", "aabbb"]
    ["aba", "bab", "xy", "abab", "bbaaa"]

regularExpressionMatchCase2 =
  regularExpressionMatchCase
    isMatch
    "a|b|c"
    ["a", "b", "c"]
    ["ab", "abc", "cba", "ac", "bb", "cc", "abcab", "xy", ""]

regularExpressionMatchParCase2 =
  regularExpressionMatchCase
    isMatchPar
    "a|b|c"
    ["a", "b", "c"]
    ["ab", "abc", "cba", "ac", "bb", "cc", "abcab", "xy", ""]

regularExpressionMatchCase3 =
  regularExpressionMatchCase
    isMatch
    "a+b+c+"
    ["abc", "aabbcc", "aaabbbccc", "aaaaaabbbbbbcccccc", "abbc"]
    ["ab", "acb", "bca", "aabccx", "aabcbccc", "abcabc", "xy", ""]

regularExpressionMatchParCase3 =
  regularExpressionMatchCase
    isMatchPar
    "a+b+c+"
    ["abc", "aabbcc", "aaabbbccc", "aaaaaabbbbbbcccccc", "abbc"]
    ["ab", "acb", "bca", "aabccx", "aabcbccc", "abcabc", "xy", ""]

regularExpressionMatchCase4 =
  regularExpressionMatchCase
    isMatch
    "[a-z]+"
    ["abc", "aabbcc", "aaabbbccc", "aaaaaabbbbbbcccccc", "abbc", "ab", "acb", "bca", "aabccx", "aabcbccc", "abcabc", "xy"]
    ["", "4234", "324", "1", "<", "#", "\\"]

regularExpressionMatchParCase4 =
  regularExpressionMatchCase
    isMatchPar
    "[a-z]+"
    ["abc", "aabbcc", "aaabbbccc", "aaaaaabbbbbbcccccc", "abbc", "ab", "acb", "bca", "aabccx", "aabcbccc", "abcabc", "xy"]
    ["", "4234", "324", "1", "<", "#", "\\"]

regularExpressionMatchCase5 =
  regularExpressionMatchCase
    isMatch
    "(a*b|c)d+(e|)"
    ["bd", "bde", "abd", "cd", "abdde", "cdde", "abde"]
    ["a", "b", "aa", "bb", "cc", "ade", "de", "xbdde", "abbddee"]

regularExpressionMatchParCase5 =
  regularExpressionMatchCase
    isMatchPar
    "(a*b|c)d+(e|)"
    ["bd", "bde", "abd", "cd", "abdde", "cdde", "abde"]
    ["a", "b", "aa", "bb", "cc", "ade", "de", "xbdde", "abbddee"]

regularExpressionMatchCase6 =
  regularExpressionMatchCase
    isMatch
    "(xy*|z)ab+(c|)"
    ["xab", "xyab", "xyyab", "zab", "xabc", "xyabc", "xyyyabc"]
    ["x", "xy", "xz", "ab", "abb", "xyyy", "abc4", "zatbc"]

regularExpressionMatchParCase6 =
  regularExpressionMatchCase
    isMatchPar
    "(xy*|z)ab+(c|)"
    ["xab", "xyab", "xyyab", "zab", "xabc", "xyabc", "xyyyabc"]
    ["x", "xy", "xz", "ab", "abb", "xyyy", "abc4", "zatbc"]

regularExpressionMatchCase7 =
  regularExpressionMatchCase
    isMatch
    "a*b+c*(d|)e+f"
    ["aabcdeef", "aabbcdeef", "abbcdef", "abbcccdeef", "abdeef"]
    ["aaaeef", "abcdf", "aabbcczdeef", "aabccddeef", "abccddef", "xy", ""]

regularExpressionMatchParCase7 =
  regularExpressionMatchCase
    isMatchPar
    "a*b+c*(d|)e+f"
    ["aabcdeef", "aabbcdeef", "abbcdef", "abbcccdeef", "abdeef"]
    ["aaaeef", "abcdf", "aabbcczdeef", "aabccddeef", "abccddef", "xy", ""]

regularExpressionMatchCase8 =
  regularExpressionMatchCase
    isMatch
    "(cat|dog|fish)+([0-9]|)"
    ["catfish", "catcatfish", "dogdogdog4", "fish3", "catdogfish0", "cat3"]
    ["catdoga", "0fishfish", "4cat4", " dogdogdogdog", "ddogfish3", "xy", ""]

regularExpressionMatchParCase8 =
  regularExpressionMatchCase
    isMatchPar
    "(cat|dog|fish)+([0-9]|)"
    ["catfish", "catcatfish", "dogdogdog4", "fish3", "catdogfish0", "cat3"]
    ["catdoga", "0fishfish", "4cat4", " dogdogdogdog", "ddogfish3", "xy", ""]

regularExpressionMatchCase9 =
  regularExpressionMatchCase
    isMatch
    "()"
    [""]
    ["catdoga", "0fishfish", "4cat4", " dogdogdogdog", "ddogfish3", "xy"]

regularExpressionMatchParCase9 =
  regularExpressionMatchCase
    isMatchPar
    "()"
    [""]
    ["catdoga", "0fishfish", "4cat4", " dogdogdogdog", "ddogfish3", "xy"]

regularExpressionMatchCase10 =
  regularExpressionMatchCase
    isMatch
    ""
    [""]
    ["catdoga", "0fishfish", "4cat4", " dogdogdogdog", "ddogfish3", "xy"]

regularExpressionMatchParCase10 =
  regularExpressionMatchCase
    isMatchPar
    ""
    [""]
    ["catdoga", "0fishfish", "4cat4", " dogdogdogdog", "ddogfish3", "xy"]

tests =
  TestLabel "Regular Expression tests" $
    TestList
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
        regularExpressionMatchCase10,
        regularExpressionMatchParCase0,
        regularExpressionMatchParCase1,
        regularExpressionMatchParCase2,
        regularExpressionMatchParCase3,
        regularExpressionMatchParCase4,
        regularExpressionMatchParCase5,
        regularExpressionMatchParCase6,
        regularExpressionMatchParCase7,
        regularExpressionMatchParCase8,
        regularExpressionMatchParCase9,
        regularExpressionMatchParCase10
      ]
