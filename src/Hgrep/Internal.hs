-- Hgrep.Internal.hs
--
--     Author: Fabian Meyer
-- Created on: 29 Jan 2018

module Hgrep.Internal where

import Options.Applicative
import Data.Semigroup ((<>))
import Regex

version = "0.1.0"

data Opts = Opts {
        nocolor :: Bool,
        fixedstr :: Bool, -- TODO
        igncase :: Bool,  -- TODO
        invmatch :: Bool,
        onlymatch :: Bool,
        quiet :: Bool,
        pattern :: String,
        files :: [String]}
        | Version

-- Parser which accepts only version flag.
versionParser :: Parser Opts
versionParser = flag' Version
    (long "version"
     <> short 'V'
     <> help "Output the version number of hgrep and exit.")

-- Parser which accepts all options.
optParser :: Parser Opts
optParser =
    Opts
    <$> switch
        (long "no-color"
         <> help "do not use colors")
    <*> switch
        (long "fixed-strings"
         <> short 'F'
         <> help "Interpret PATTERN as a list of fixed strings (instead of regular expressions), separated by newlines, any of which is to be matched.")
    <*> switch
        (long "ignore-case"
         <> short 'i'
         <> help "Ignore case distinctions in both the PATTERN and the input files.")
    <*> switch
        (long "invert-match"
        <> short 'v'
        <> help "Invert the sense of matching, to select non-matching lines.")
    <*> switch
        (long "only-matching"
        <> short 'o'
        <> help "Print only the matched (non-empty) parts of a matching line, with each such part on a separate output line.")
    <*> switch
        (long "quiet"
        <> short 'q'
        <> help "Quiet; do not write anything to standard output.  Exit immediately with zero status if any  match  is found, even if an error was detected.")
    <*> argument str (metavar "PATTERN")
    <*> some
        (argument str (metavar "[FILE]..."))

-- Either accept version or all other options.
fullParser :: Parser Opts
fullParser = versionParser <|> optParser

-- Start point of hgrep. Actually main.
run :: IO ()
run = runSearch =<< execParser opts
    where opts = info (fullParser <**> helper)
                 (fullDesc
                  <> progDesc "Search for PATTERN in each FILE.")

-- Print version if flag was set. Otherwise start search in fixed string or
-- regex mode.
runSearch :: Opts -> IO ()
runSearch Version = printVersion
runSearch o | fixedstr o = searchFixed o
runSearch o              = searchRegex o

-- Print version text on stdout.
printVersion :: IO ()
printVersion = putStrLn ("hgrep " ++ version ++ "\nCopyright Fabian Meyer 2018")

-- Compute fixed string search.
searchFixed :: Opts -> IO ()
searchFixed = undefined

-- Compute regex search. Compiles the given regex patter.
searchRegex :: Opts -> IO ()
searchRegex o = case compile (pattern o) of
    Nothing -> fail ("Invalid pattern " ++ pattern o)
    Just r  -> mapM_ (matchFile o r) (files o)

-- Check for matches in the given file.
matchFile :: Opts -> Regex -> String -> IO ()
matchFile o r f = do
    fcontent <- readFile f
    matchLines o r (lines fcontent)

-- Check for matches per line. Distinguish if hgrep runs in inverted mode or not.
matchLines :: Opts -> Regex -> [String] -> IO ()
matchLines o r | invmatch o = mapM_ (matchLnInv r)
matchLines o r              = mapM_ (matchLn o r)

-- Matches a line according to inverted mode. If the line produces no match
-- then print it to stdout.
matchLnInv :: Regex -> String -> IO ()
matchLnInv r l = case matchAll r l of
        [] -> putStrLn l
        ms -> return ()

-- Matches a line. If the line produces a match then print the line to stdout.
matchLn :: Opts -> Regex -> String -> IO ()
matchLn o r l = case matchAll r l of
        [] -> return ()
        ms -> printMatches o l ms

-- Print a given match to stdout. Distinguish between normal and nocolor mode.
printMatches :: Opts -> String -> [(Int, String)] -> IO ()
printMatches o l ms | quiet o     = return ()
                    | nocolor o   = putStrLn l
                    | onlymatch o = mapM_ (\(_,s) -> putStrLn s) ms
                    | otherwise   = printMatchesCol l ms

printMatchesCol :: String -> [(Int, String)] -> IO ()
printMatchesCol l ms = printStr . foldr colorMatch (0,"") $ ms
    where
    remainStr n mlen llen con = substr (n + mlen) (llen-n-mlen-con) -- calc remainng str after match
    outStr n m s con = (colorStr m) ++ remainStr n (length m) (length l) con l ++ s -- build full result str
    colorMatch (n,m) (con,s) = ((length l) - n, outStr n m s con) -- update consumed chars and result str
    printStr (con, s) = putStrLn ((take (length l - con) l) ++ s)

substr :: Int -> Int -> String -> String
substr b l = take l . drop b

bashRed = "\x1b[1;31m"
bashNc = "\x1b[0m"

colorStr :: String -> String
colorStr s = bashRed ++ s ++ bashNc
