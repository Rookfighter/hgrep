-- RegEx.Internal.hs
--
--     Author: Fabian Meyer
-- Created on: 25 Jan 2018

module Regex.Internal where

import ParserCon

type Regex = Parser Char String

compile :: String -> Maybe Regex
compile = parse reFull

match :: Regex -> String -> Maybe String
match = parse

reFull :: Parser Char Regex
reFull = reAlt

reMany :: Parser Char Regex -> Parser Char Regex
reMany p = g <$> p
    where g rp = concat <$> pmany rp

reSome :: Parser Char Regex -> Parser Char Regex
reSome p = g <$> p
    where g rp = concat <$> psome rp

-- Accepts alternatives of regular expressions.
-- Corresponds to re0 of exercise sheet.
reAlt :: Parser Char Regex
reAlt =
    g
    <$> re1
    <*> pmany (pure id
            <*  lit '|'
            <*> re1)
    where g p ps = foldr (<|>) p ps

-- Accepts sequences of regex. At least 1.
re1 :: Parser Char Regex
re1 = g <$> pmany reQuant <*> reQuant
    where g ps p = foldr (\a b -> (++) <$> a <*> b) p ps

-- Accepts quantifictations of the regex.
-- Corresponds to re2 of exercise sheet.
reQuant :: Parser Char Regex
reQuant =
    (reMany $
        reAtom
        <* lit '*')
    <|>
    (reSome $
        reAtom
        <* lit '+')
    <|>
    reAtom

-- Accepts atomics in parenthesis for grouping.
reParen :: Parser Char Regex
reParen = pure id
    <* lit '('
    <*> reAlt
    <* lit ')'

-- Accepts atomic regex which are literals, escape sequences, dots and atomics
-- in parenthesis.
reAtom :: Parser Char Regex
reAtom = reLit <|> reEsc <|> reDot <|> reParen

-- Accepts literals of regex, which are all chars without special meaning.
-- Excluding "()+$*.|\\[]"
reLit :: Parser Char Regex
reLit = try g
    where g c | elem c specChars = Nothing
              | otherwise        = Just $ (return <$> lit c) -- accept every char
                                                             -- except special ones

-- Accepts escaped chars of regex which have special meaning.
reEsc :: Parser Char Regex
reEsc = pure id <* lit '\\' <*> try g
    where g c | elem c specChars = Just $ (return <$> lit c) -- accept the escaped char
          g c | otherwise        = Nothing

-- Accepts dot and creates regex that accepts any char.
reDot :: Parser Char Regex
reDot = try g
    where g '.' = Just $ return  <$> satisfy (\_ -> True) -- accept every char
          g _   = Nothing

specChars = "[]()+^$*.|\\"
