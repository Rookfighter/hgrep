-- RegexSpec.hs
--
--     Author: Fabian Meyer
-- Created on: 07 Feb 2018

module RegexSpec (tests) where

import Regex.Internal
import Test.HUnit

tests = [
    TestLabel "Test compileParen" testCompileParen,
    TestLabel "Test compileBracket" testCompileBracket,
    TestLabel "Test compileRange" testCompileRange,
    TestLabel "Test regexAlpha" testRegexAlpha,
    TestLabel "Test regexNum" testRegexNum,
    TestLabel "Test regexWhiteSpace" testRegexWhiteSpace,
    TestLabel "Test regexOptStr" testRegexOptStr
    ]

testCompileParen :: Test
testCompileParen = TestCase (do
    assertCompile "" comp "(ab)"
    assertMatch "(ab)" (reg "(ab)") "ab"
    assertNoMatch "(ab)" (reg "(ab)") "b"
    assertNoMatch "(ab)" (reg "(ab)") "a"
    assertNoMatch "(ab)" (reg "(ab)") "abb"

    assertNoCompile "" comp "ab"
    assertNoCompile "" comp "(ab"
    assertNoCompile "" comp "ab)"
    )
    where comp  = reParen
          reg s = compileTrusted comp s

testCompileBracket :: Test
testCompileBracket = TestCase (do
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
    )
    where comp  = reBracket
          reg s = compileTrusted comp s

testCompileRange :: Test
testCompileRange = TestCase (do
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
    where comp  = reRange
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
