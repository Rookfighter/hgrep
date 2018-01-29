-- RegEx.hs
--
--     Author: Fabian Meyer
-- Created on: 25 Jan 2018

module Regex (
    Regex,
    compile,
    match,
    matchAll
    ) where


import qualified Regex.Internal as R

type Regex = R.Regex

-- Compile the given string into a Regex parser.
compile :: String -> Maybe Regex
compile = R.compile

-- Match the given string against a Regex parser.
match :: Regex -> String -> Maybe (Int, String)
match = R.match

matchAll :: Regex -> String -> [(Int, String)]
matchAll = R.matchAll
