-- RegexSpec.hs
--
--     Author: Fabian Meyer
-- Created on: 07 Feb 2018

module RegexSpec (tests) where

import Regex.Internal
import Test.HUnit

tests = [
    TestLabel "Test compilerFull" testCompilerFull,
    TestLabel "Test compilerQuant" testCompilerQuant,
    TestLabel "Test compilerGenRep" testCompilerGenRep,
    TestLabel "Test compilerAtom" testCompilerAtom,
    TestLabel "Test compilerLit" testCompilerLit,
    TestLabel "Test compilerEsc" testCompilerEsc,
    TestLabel "Test compilerDot" testCompilerDot,
    TestLabel "Test compilerParen" testCompilerParen,
    TestLabel "Test compilerBracket" testCompilerBracket,
    TestLabel "Test compilerRange" testCompilerRange,
    TestLabel "Test regexAlpha" testRegexAlpha,
    TestLabel "Test regexNum" testRegexNum,
    TestLabel "Test regexWhiteSpace" testRegexWhiteSpace,
    TestLabel "Test regexOptStr" testRegexOptStr
    ]

testCompilerFull :: Test
testCompilerFull = TestCase (do
    assertCompile "" comp "a[abc]"
    assertNoMatch "a[abc]" (reg "a[bcd]") "a"
    assertMatch "a[abc]" (reg "a[bcd]") "ab"
    assertMatch "a[abc]" (reg "a[bcd]") "ac"
    assertMatch "a[abc]" (reg "a[bcd]") "ad"
    assertNoMatch "a[abc]" (reg "a[bcd]") "ae"

    assertCompile "" comp "[abc]"
    assertMatch "[abc]" (reg "[abc]") "a"
    assertMatch "[abc]" (reg "[abc]") "b"
    assertMatch "[abc]" (reg "[abc]") "c"
    assertNoMatch "[abc]" (reg "[abc]") "d"
    assertNoMatch "[abc]" (reg "[abc]") "aa"
    )
    where comp  = compilerFull
          reg s = compileTrusted comp s

testCompilerQuant :: Test
testCompilerQuant = TestCase (do
    assertCompile "" comp "a*"
    assertCompile "" comp "a?"
    assertCompile "" comp "a+"
    assertNoCompile "" comp "*"
    assertNoCompile "" comp "?"
    assertNoCompile "" comp "+"

    assertMatch "a*" (reg "a*") "a"
    assertMatch "a*" (reg "a*") "aaa"
    assertMatch "a*" (reg "a*") ""

    assertMatch "a?" (reg "a?") "a"
    assertMatch "a?" (reg "a?") ""
    assertNoMatch "a?" (reg "a?") "aa"

    assertMatch "a+" (reg "a+") "a"
    assertMatch "a+" (reg "a+") "aaa"
    assertNoMatch "a+" (reg "a+") ""
    )
    where comp = compilerQuant
          reg s = compileTrusted comp s

testCompilerGenRep :: Test
testCompilerGenRep = TestCase (do
    assertCompile "" comp "a{2,4}"
    assertCompile "" comp "a{2,0}"

    assertMatch "a{2,4}" (reg "a{2,4}") "aa"
    assertMatch "a{2,4}" (reg "a{2,4}") "aaa"
    assertMatch "a{2,4}" (reg "a{2,4}") "aaaa"
    assertNoMatch "a{2,4}" (reg "a{2,4}") "a"
    assertNoMatch "a{2,4}" (reg "a{2,4}") "aaaaa"

    assertNoMatch "a{2,0}" (reg "a{2,0}") "a"
    assertMatch "a{2,0}" (reg "a{2,0}") ""
    )
    where comp = compilerGenRep
          reg s = compileTrusted comp s

testCompilerAtom :: Test
testCompilerAtom = TestCase (do
    assertCompile "lit" comp "a"
    assertCompile "esc" comp "\\d"
    assertCompile "dot" comp "."
    assertCompile "paren" comp "(ab)"
    assertCompile "bracket" comp "[ab]"
    assertCompile "bracket" comp "[^ab]"
    )
    where comp  = compilerAtom
          reg s = compileTrusted comp s

testCompilerLit :: Test
testCompilerLit = TestCase (do
    mapM_ (\c -> assertNoCompile "" comp [c]) specChars

    assertMatch "" (reg "a") "a"
    assertNoMatch "" (reg "a") "b"
    )
    where comp  = compilerLit
          reg s = compileTrusted comp s

testCompilerEsc :: Test
testCompilerEsc = TestCase (do
    assertCompile "digit" comp "\\d"
    mapM_ (\c -> assertMatch "digit" (reg "\\d") [c]) "0123456789"
    assertNoMatch "digit" (reg "\\d") "10"

    assertCompile "letter" comp "\\l"
    mapM_ (\c -> assertMatch "letter" (reg "\\l") [c]) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    assertNoMatch "digit" (reg "\\d") "ab"

    assertCompile "whitespace" comp "\\w"
    mapM_ (\c -> assertMatch "whitespace" (reg "\\w") [c]) " \t\v\n\r\b"
    assertNoMatch "digit" (reg "\\d") "  "

    mapM_ (\c -> assertCompile "special" comp ('\\':[c])) specChars
    mapM_ (\(c,esc) -> assertMatch "special" (reg esc) [c]) . map (\c -> (c,'\\':[c])) $ specChars
    )
    where comp  = compilerEsc
          reg s = compileTrusted comp s

testCompilerDot :: Test
testCompilerDot = TestCase (do
    assertCompile "" comp "."
    assertNoCompile "" comp "as"

    mapM_ (\c -> assertMatch "" (reg ".") [c]) "!§$%&/()=?#+*abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    )
    where comp  = compilerDot
          reg s = compileTrusted comp s

testCompilerParen :: Test
testCompilerParen = TestCase (do
    assertCompile "" comp "(ab)"
    assertMatch "(ab)" (reg "(ab)") "ab"
    assertNoMatch "(ab)" (reg "(ab)") "b"
    assertNoMatch "(ab)" (reg "(ab)") "a"
    assertNoMatch "(ab)" (reg "(ab)") "abb"

    assertNoCompile "" comp "ab"
    assertNoCompile "" comp "(ab"
    assertNoCompile "" comp "ab)"
    )
    where comp  = compilerParen
          reg s = compileTrusted comp s

testCompilerBracket :: Test
testCompilerBracket = TestCase (do
    assertCompile "" comp "[abc]"
    assertMatch "[abc]" (reg "[abc]") "a"
    assertMatch "[abc]" (reg "[abc]") "b"
    assertMatch "[abc]" (reg "[abc]") "c"
    mapM_ (\c -> assertNoMatch "[abc]" (reg "[abc]") [c]) "defghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

    assertCompile "" comp "[^abc]"
    mapM_ (\c -> assertMatch "[^abc]" (reg "[^abc]") [c]) "defghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    assertNoMatch "[^abc]" (reg "[^abc]") "a"
    assertNoMatch "[^abc]" (reg "[^abc]") "b"
    assertNoMatch "[^abc]" (reg "[^abc]") "c"

    assertNoCompile "" comp "[a[b]"
    assertNoCompile "" comp "[abc"
    assertNoCompile "" comp "abc]"
    assertNoCompile "" comp "[]"
    )
    where comp  = compilerBracket
          reg s = compileTrusted comp s

testCompilerRange :: Test
testCompilerRange = TestCase (do
    assertCompile "compile a-z" comp "a-z"
    mapM_ (\c -> assertMatch "match a-z" (reg "a-z") [c]) "abcdefghijklmnopqrstuvwxyz"
    mapM_ (\c -> assertNoMatch "no match a-z" (reg "a-z") [c]) "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

    assertCompile "compile A-Z" comp "A-Z"
    mapM_ (\c -> assertMatch "match A-Z" (reg "A-Z") [c]) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    mapM_ (\c -> assertNoMatch "no match A-Z" (reg "A-Z") [c]) "abcdefghijklmnopqrstuvwxyz0123456789"

    assertCompile "compile a-Z" comp "a-Z"
    mapM_ (\c -> assertMatch "match a-Z" (reg "a-Z") [c]) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    mapM_ (\c -> assertNoMatch "no match a-Z" (reg "a-Z") [c]) "0123456789"

    assertCompile "compile 0-9" comp "0-9"
    mapM_ (\c -> assertMatch "match 0-9" (reg "0-9") [c]) "0123456789"
    mapM_ (\c -> assertNoMatch "no match 0-9" (reg "0-9") [c]) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    where comp  = compilerRange
          reg s = compileTrusted comp s

testRegexAlpha :: Test
testRegexAlpha = TestCase (do
    mapM_ (\c -> assertMatch "match alpha" reg [c]) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    mapM_ (\c -> assertNoMatch "no match alpha" reg [c]) "0123456789!$%&/(=?_:;,.+#')")
    where reg = regexAlpha

testRegexNum :: Test
testRegexNum = TestCase (do
    mapM_ (\c -> assertMatch "match num" reg [c]) "0123456789"
    mapM_ (\c -> assertNoMatch "no match num" reg [c]) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!$%&/(=?_:;,.+#')")
    where reg = regexNum

testRegexWhiteSpace :: Test
testRegexWhiteSpace = TestCase (do
    mapM_ (\c -> assertMatch "match whitespace" reg [c]) " \t\v\n\r\b"
    mapM_ (\c -> assertNoMatch "no match whitespace" reg [c]) "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!$%&/(=?_:;,.+#')")
    where reg = regexWhiteSpace

testRegexOptStr :: Test
testRegexOptStr = TestCase (do
    assertMatch "" reg "a"
    assertMatch "" reg "b"
    assertMatch "" reg "c"
    assertNoMatch "" reg "ab"
    assertNoMatch "" reg "abc"
    assertNoMatch "" reg "abc")
    where reg = regexOptStr "abc"

-------------------------------------
-- Assertions
-------------------------------------

-- assertCompileMatch :: String -> String -> Assertion
-- assertCompileMatch pattern str = do
--     assertCompile pattern
--     assertMatch regex str
--     where (Just regex) = compile pattern
--
-- assertCompileNoMatch :: String -> String -> Assertion
-- assertCompileNoMatch pattern  str = do
--     assertCompile pattern
--     assertNoMatch regex str
--     where (Just regex) = compile pattern

assertCompile :: String -> Compiler -> String -> Assertion
assertCompile pre comp pattern =
    (compile2bool comp pattern)
    @?
    (pre ++ ": Expected to compile \"" ++ pattern ++ "\"")

assertNoCompile :: String -> Compiler -> String -> Assertion
assertNoCompile pre comp pattern =
    (not $ compile2bool comp pattern)
    @?
    (pre ++ ": Expected not to compile \"" ++ pattern ++ "\"")

compile2bool :: Compiler -> String -> Bool
compile2bool comp pattern = maybe2bool $ compileCompiler comp pattern

assertMatch :: String -> Regex -> String -> Assertion
assertMatch pre regex str =
    (match2bool regex str)
    @?
    (pre ++ ": Expected to find match for \"" ++ str ++ "\"")

assertNoMatch :: String -> Regex -> String -> Assertion
assertNoMatch pre regex str =
    (not $ match2bool regex str)
    @?
    (pre ++ ": Expected to find no match for \"" ++ str ++ "\"")

match2bool :: Regex -> String -> Bool
match2bool regex str = maybe2bool $ matchExact regex str

maybe2bool :: Maybe a -> Bool
maybe2bool (Just _) = True
maybe2bool Nothing  = False
