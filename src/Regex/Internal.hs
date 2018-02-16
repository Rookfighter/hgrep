-- RegEx.Internal.hs
--
--     Author: Fabian Meyer
-- Created on: 25 Jan 2018

module Regex.Internal where

import ParserCon
import Data.Char

type Preprocessor = Parser Char String
-- A Regex parses a given string to match it against its underlying pattern.
type RegexRaw = Parser Char String
data Regex = Regex RegexRaw String

instance Eq Regex where
    (==) (Regex _ s1) (Regex _ s2) = s1 == s2

instance Show Regex where
    show (Regex _ s) = s

-- A Compiler parses a given regex pattern and creates a representing parser.
type Compiler = Parser Char RegexRaw

test :: (String, [String]) -> IO ()
test (pat, strs)= case compile pat of
    Just reg -> mapM_ (testg reg) strs
    Nothing -> putStrLn "Compile Error."
testg reg s = case match reg s of
        Just p  -> putStrLn ("Found match for " ++ s ++ " at " ++ show p);
        Nothing -> putStrLn ("No match for " ++ s)

preprocess :: Preprocessor -> String -> String
preprocess p s = result
    where Just result = parse p s

preprocIgnCase :: Preprocessor
preprocIgnCase =
        bracketStr
        <$> litStr '['
        <*> (concat <$> pmany (preprocUpperLower <|> litId))
        <*> litStr ']'
    <|>
        concat
        <$> pmany (preprocCaseAlt <|> litId)
    where litStr c = return <$> lit c
          litId  = return <$> satisfy (\_ -> True)
          bracketStr s1 s2 s3 = s1 ++ s2 ++ s3

preprocCaseAlt :: Preprocessor
preprocCaseAlt = wrapBracket <$> preprocUpperLower
    where wrapBracket s = "[" ++ s ++ "]"

preprocUpperLower :: Preprocessor
preprocUpperLower = try upperLower
    where upperLower c | elem c (alphaLow ++ alphaUp) = Just $ (toLower c):[toUpper c]
                       | otherwise                    = Nothing


-- Finds and matches all occurences of the given reges in given string.
matchAll :: Regex -> String -> [(Int, String)]
matchAll = matchAllR 0

-- Recursive implementation of matchAll to extract start index of found strings.
matchAllR :: Int -> Regex -> String -> [(Int, String)]
matchAllR start reg "" = []
matchAllR start reg str = case match reg str of
    Nothing        -> []
    Just (idx,mat) -> let nextStart = start + idx + (max 1 $ length mat)
                          nextStr   = drop (idx + (max 1 $ length mat)) str
                      in
                      (start+idx,mat) : matchAllR nextStart reg nextStr

-- Finds and matches first occurence of the given regex in given string.
match :: Regex -> String -> Maybe (Int, String)
match (Regex reg _) = matchR 0 reg

-- Recursive implementation of match to extract start index of found string.
matchR :: Int -> RegexRaw -> String -> Maybe (Int, String)
matchR n reg xs =
    -- append many regexAny to consume every token after match
    case parse (reg <* regexMany regexAny) xs of
        Just s  -> Just (n, s)
        Nothing -> matchNext n reg xs
    where matchNext n reg []     = Nothing
          matchNext n reg (x:xs) = matchR (n+1) reg xs

matchExact :: Regex -> String -> Maybe String
matchExact (Regex reg _) = parse reg

-- Compile the given pattern into a regex.
compile :: String -> Maybe Regex
compile = compileCompiler compilerFull

compileCompiler :: Compiler -> String -> Maybe Regex
compileCompiler comp pat = fmap (\reg -> Regex reg pat) (parse comp pat)

compileTrusted :: Compiler -> String -> Regex
compileTrusted comp pat = reg
    where (Just reg) = compileCompiler comp pat

-- Full regex parser.
compilerFull :: Compiler
compilerFull = compilerAlt

-- Accepts alternatives of regular expressions.
-- Corresponds to re0 of exercise sheet.
compilerAlt :: Compiler
compilerAlt =
    foldOpt
    <$> compilerSeq
    <*> pmany (pure id
            <*  lit '|'
            <*> compilerSeq)
    where foldOpt p ps = regexFoldOpt (p:ps)

-- Accepts sequences of regex. At least 1.
compilerSeq :: Compiler
compilerSeq = regexFoldSeq <$> psome compilerQuant

regexFoldSeq :: [RegexRaw] -> RegexRaw
regexFoldSeq regs = foldr regexSeq (pure "") regs

-- Accepts quantifications of the regex.
-- Corresponds to re2 of exercise sheet.
compilerQuant :: Compiler
compilerQuant =
    (regexMany <$>
        compilerAtom
        <* lit '*')
    <|>
    (regexSome <$>
        compilerAtom
        <* lit '+')
    <|>
    (regexOpt <$>
        compilerAtom
        <* lit '?')
    <|>
    compilerGenRep
    <|>
    compilerAtom

-- Accepts generic repetition regex, e.g. x{1,3}.
compilerGenRep :: Compiler
compilerGenRep = regexGenRep
    <$> compilerAtom
    <*  lit '{'
    <*> parserStr2Int
    <*  lit ','
    <*> parserOptInt
    <*  lit '}'

regexGenRep :: RegexRaw -> Int -> (Maybe Int) -> RegexRaw
regexGenRep reg n1 Nothing   | n1 <= 0   = regexMany reg
                             | otherwise = regexSeq reg (regexGenRep reg (n1-1) Nothing)
regexGenRep reg n1 (Just n2) | n2 <= 0   = pure ""
                             | n1 <= 0   = regexSeq (reg <|> pure "") (regexGenRep reg 0 (Just $ n2-1))
                             | otherwise = regexSeq reg (regexGenRep reg (n1-1) (Just $ n2-1))

regexSeq :: RegexRaw -> RegexRaw -> RegexRaw
regexSeq reg1 reg2 = (++) <$> reg1 <*> reg2

parserOptInt :: Parser Char (Maybe Int)
parserOptInt =
    (Just <$> parserStr2Int)
    <|>
    ((\_ -> Nothing) <$> pure "")

parserStr2Int :: Parser Char Int
parserStr2Int = read
    <$> (psome $ satisfy (\c -> elem c digits))

regexMany :: RegexRaw -> RegexRaw
regexMany reg = (reg >>= g) <|> pure ""
    where g "" = return <$> satisfy (\_ -> False)
          g s  = (s++) <$> regexMany reg

regexSome :: RegexRaw -> RegexRaw
regexSome reg = reg >>= g
    where g "" = pure ""
          g s  = (s++) <$> regexMany reg

-- Allows the found regex pattern to be applied zero or one time.
regexOpt :: RegexRaw -> RegexRaw
regexOpt reg = reg <|> pure ""

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
    where g '.' = Just regexAny
          g _   = Nothing

-- Regex that matches any token.
regexAny :: RegexRaw
regexAny = return <$> satisfy (\_ -> True)

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
    <*> compilerBracketInner
    <*  lit ']'

compilerBracketInner :: Compiler
compilerBracketInner = compilerBracketTerm <|> compilerBracketTermInv

-- Accepts non inverted terms in brackets, which are all kind of literals.
-- No escapes and no special meanings for chars are allowed in brackets.
compilerBracketTerm :: Compiler
compilerBracketTerm = compilerSomeOpt $ compilerRange <|> try g
    where g c | elem c "^[]" = Nothing
              | otherwise    = Just $ (return <$> lit c) -- accept every char

-- Allows the found regex pattern to be applied one or more times optionally.
compilerSomeOpt :: Compiler -> Compiler
compilerSomeOpt comp = regexFoldOpt <$> psome comp

regexFoldOpt :: [RegexRaw] -> RegexRaw
regexFoldOpt (r:rs) = foldr (<|>) r rs

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
regexAlphaLow :: RegexRaw
regexAlphaLow = regexOptStr alphaLow

-- Regex which accepts any uppercase case letter
regexAlphaUp :: RegexRaw
regexAlphaUp = regexOptStr alphaUp

-- Regex which accepts any lower or upper case letter
regexAlpha :: RegexRaw
regexAlpha = regexAlphaLow <|> regexAlphaUp

-- Regex which accepts any digit.
regexNum :: RegexRaw
regexNum = regexOptStr digits

-- Regex which accepts any letter and digit.
regexAlphaNum :: RegexRaw
regexAlphaNum = regexAlpha <|> regexNum

-- Regex which accepts any white space.
regexWhiteSpace :: RegexRaw
regexWhiteSpace = regexOptStr " \t\v\n\r\b"

-- Helper function to convert a given string into a regex that accepts
-- any char of the string.
regexOptStr :: String -> RegexRaw
regexOptStr = foldr (<|>) empty . map (\c -> return <$> lit c)

-- Defines characters that have a special meaning
specChars = "{}?[]()+^$*.|\\"
alphaLow = "abcdefghijklmnopqrstuvwxyz"
alphaUp = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
digits = "0123456789"
