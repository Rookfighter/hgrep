-- RegEx.Internal.hs
--
--     Author: Fabian Meyer
-- Created on: 25 Jan 2018

module Regex.Internal where

import ParserCon

-- A Regex parses a given string to match it against its underlying pattern.
type Regex = Parser Char String
-- A Compiler parses a given regex pattern and creates a representing parser.
type Compiler = Parser Char Regex

-- Finds and matches all occurences of the given reges in given string.
matchAll :: Regex -> String -> [(Int, String)]
matchAll = matchAllR 0

-- Recursive implementation of matchAll to extract start index of found strings.
matchAllR :: Int -> Regex -> String -> [(Int, String)]
matchAllR n r s = case match r s of
    Nothing     -> []
    Just (n2,s2) -> (n+n2,s2) : matchAllR (n+n2+length s2) r (drop (n2 + length s2) s)

-- Finds and matches first occurence of the given regex in given string.
match :: Regex -> String -> Maybe (Int, String)
match  = matchR 0

-- Recursive implementation of match to extract start index of found string.
matchR :: Int -> Regex -> String -> Maybe (Int, String)
matchR n r []     = Nothing
matchR n r (x:xs) =
    -- append regexAny to consume every token after match
    case parse (r <* regexAny) (x:xs) of
        Just s  -> Just (n, s)
        Nothing -> matchR (n+1) r xs

matchExact :: Regex -> String -> Maybe String
matchExact = parse

-- Regex that matches any token zero more times.
-- Used to consume remaining tokens after a match.
regexAny :: Regex
regexAny = pmany $ satisfy (\_ -> True)

-- Compile the given pattern into a regex.
compile :: String -> Maybe Regex
compile = compileCompiler compilerFull

compileCompiler :: Compiler -> String -> Maybe Regex
compileCompiler = parse

compileTrusted :: Compiler -> String -> Regex
compileTrusted comp pattern = regex
    where (Just regex) = compileCompiler comp pattern

-- Full regex parser.
compilerFull :: Compiler
compilerFull = compilerAlt

-- Accepts alternatives of regular expressions.
-- Corresponds to re0 of exercise sheet.
compilerAlt :: Compiler
compilerAlt =
    g
    <$> compilerSeq
    <*> pmany (pure id
            <*  lit '|'
            <*> compilerSeq)
    where g p ps = foldr (<|>) p ps

-- Accepts sequences of regex. At least 1.
compilerSeq :: Compiler
compilerSeq = g <$> psome compilerQuant
    where g ps = foldr (\a b -> (++) <$> a <*> b) (pure "") ps

-- Accepts quantifictations of the regex.
-- Corresponds to re2 of exercise sheet.
compilerQuant :: Compiler
compilerQuant =
    (compilerMany $
        compilerAtom
        <* lit '*')
    <|>
    (compilerSome $
        compilerAtom
        <* lit '+')
    <|>
    (compilerOptional $
        compilerAtom
        <* lit '?')
    <|>
    compilerGenRep
    <|>
    compilerAtom

-- Accepts generic repetition regex, e.g. x{1,3}.
compilerGenRep :: Compiler
compilerGenRep = g
    <$> compilerAtom
    <*  lit '{'
    <*> parserStr2Int
    <*  lit ','
    <*> parserStr2Int
    <*  lit '}'
    where g rp n1 n2 | n2 <= 0   = pure ""
                     | n1 <= 0   = (++) <$> (rp <|> pure "") <*> g rp 0 (n2-1)
                     | otherwise = (++) <$> rp <*> g rp (n1-1) (n2-1)

parserStr2Int :: Parser Char Int
parserStr2Int = read
    <$> (psome $ satisfy (\c -> elem c "0123456789"))

-- Allows the found regex pattern to be applied zero or many times.
compilerMany :: Compiler -> Compiler
compilerMany p = g <$> p
    where g rp = concat <$> pmany rp

-- Allows the found regex pattern to be applied one or many times.
compilerSome :: Compiler -> Compiler
compilerSome p = g <$> p
    where g rp = concat <$> psome rp

-- Allows the found regex pattern to be applied zero or one time.
compilerOptional :: Compiler -> Compiler
compilerOptional p = g <$> p
    where g r = r <|> pure ""

-- Accepts atomic regex which are literals, escape sequences, dots and atomics
-- in parenthesis.
compilerAtom :: Compiler
compilerAtom = compilerLit <|> compilerEsc <|> compilerDot <|> compilerParen <|> compilerBracket

-- Accepts literals of regex, which are all chars without special meaning.
-- Excluding "()+$*.|\\[]"
compilerLit :: Compiler
compilerLit = try g
    where g c | elem c specChars = Nothing
              | otherwise        = Just $ (return <$> lit c) -- accept every char
                                                             -- except special ones

-- Accepts escaped chars of regex which have special meaning.
compilerEsc :: Compiler
compilerEsc = pure id <* lit '\\' <*> try g
    where g c | elem c specChars = Just $ (return <$> lit c) -- accept the escaped char
          g 'd' = Just $ regexNum
          g 'l' = Just $ regexAlpha
          g 'w' = Just $ regexWhiteSpace
          g c   = Nothing

-- Accepts dot and creates regex that accepts any char.
compilerDot :: Compiler
compilerDot = try g
    where g '.' = Just $ return <$> satisfy (\_ -> True) -- accept every char
          g _   = Nothing

-- Accepts atomics in parenthesis for grouping.
compilerParen :: Compiler
compilerParen = pure id
    <*  lit '('
    <*> compilerAlt
    <*  lit ')'

-- Accepts alternative literals in brackets and their inverse.
-- E.g. "[abc]" and "[^abc]"
compilerBracket :: Compiler
compilerBracket = pure id
    <*  lit '['
    <*> (compilerBracketTerm <|> compilerBracketTermInv)
    <*  lit ']'

-- Accepts non inverted terms in brackets, which are all kind of literals.
-- No escapes and no special meanings for chars are allowed in brackets.
compilerBracketTerm :: Compiler
compilerBracketTerm = compilerSomeOptional $ compilerRange <|> try g
    where g c | elem c "^[]" = Nothing
              | otherwise    = Just $ (return <$> lit c) -- accept every char

-- Allows the found regex pattern to be applied one or more times optionally.
compilerSomeOptional :: Compiler -> Compiler
compilerSomeOptional p = g <$> psome p
    where g ps = foldr (<|>) (pure "") ps

-- Accepts inverted terms in brackets. See also compilerBracketTerm.
-- Inverted terms always start with '^'.
compilerBracketTermInv :: Compiler
compilerBracketTermInv = pure id
    <*  lit '^'
    <*> pure g
    <*> (psome . satisfy $ (\c -> not . elem c $ "^[]"))
    where g s = try (h s)
          h s c | elem c s = Nothing
          h s c            = Just $ return c

-- Accepts predefined ranges, such as numbers and lowercase / uppercase alphabet.
compilerRange :: Compiler
compilerRange =
      (pure regexAlphaLow
       <* lit 'a'
       <* lit '-'
       <* lit 'z')
  <|>
      (pure regexAlphaUp
       <* lit 'A'
       <* lit '-'
       <* lit 'Z')
  <|>
      (pure regexAlpha
       <* lit 'a'
       <* lit '-'
       <* lit 'Z')
  <|>
      (pure regexNum
       <* lit '0'
       <* lit '-'
       <* lit '9')

-- Regex which accepts any lower case letter.
regexAlphaLow :: Regex
regexAlphaLow = regexOptStr "abcdefghijklmnopqrstuvwxyz"

-- Regex which accepts any uppercase case letter
regexAlphaUp :: Regex
regexAlphaUp = regexOptStr "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- Regex which accepts any lower or upper case letter
regexAlpha :: Regex
regexAlpha = regexAlphaLow <|> regexAlphaUp

-- Regex which accepts any digit.
regexNum :: Regex
regexNum = regexOptStr "0123456789"

-- Regex which accepts any letter and digit.
regexAlphaNum :: Regex
regexAlphaNum = regexAlpha <|> regexNum

-- Regex which accepts any white space.
regexWhiteSpace :: Regex
regexWhiteSpace = regexOptStr " \t\v\n\r\b"

-- Helper function to convert a given string into a regex that accepts
-- any char of the string.
regexOptStr :: String -> Regex
regexOptStr = foldr (<|>) empty . map (\c -> return <$> lit c)

-- Defines characters that have a special meaning
specChars = "{}?[]()+^$*.|\\"
