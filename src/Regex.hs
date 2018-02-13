-- RegEx.hs
--
--     Author: Fabian Meyer
-- Created on: 25 Jan 2018

module Regex (
    Regex,
    Preprocessor,
    preprocess,
    ignCase,
    compile,
    match,
    matchAll
    ) where


import qualified Regex.Internal as R

type Regex = R.Regex
type Preprocessor = R.Preprocessor

ignCase :: Preprocessor
ignCase = R.preprocIgnCase

preprocess :: Preprocessor -> String -> String
preprocess = R.preprocess

-- Compile the given string into a Regex parser.
compile :: String -> Maybe Regex
compile = R.compile

-- Match the given string against a Regex parser.
match :: Regex -> String -> Maybe (Int, String)
match = R.match

-- Find all matches in the given string.
matchAll :: Regex -> String -> [(Int, String)]
matchAll = R.matchAll
