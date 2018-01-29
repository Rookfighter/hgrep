-- Hgrep.Internal.hs
--
--     Author: Fabian Meyer
-- Created on: 29 Jan 2018

module Hgrep.Internal where

import Options.Applicative
import Data.Semigroup ((<>))

data Opts = Opts {
        quiet :: Bool,
        pattern :: String,
        files :: [String]}

optParser :: Parser Opts
optParser = Opts
    <$> switch
        (long "quiet"
         <> short 'q'
         <> help "run in quiet mode")
    <*> argument str (metavar "PATTERN")
    <*> some
        (argument str (metavar "[FILE]..."))

run :: IO ()
run = runSearch =<< execParser opts
    where opts = info (optParser <**> helper)
                 (fullDesc
                  <> progDesc "Search for PATTERN in each FILE or standard input.")

runSearch :: Opts -> IO ()
runSearch o = case compile (pattern o) of
                Nothing -> fail "Inval pattern"
                Just r  -> map searchFile (files o)
    where
    searchFile f = do {
        input <- readFile f;
        map searchLine (lines input)
        }
    searchLine l = map printMatch $ match r l
    printMatch l m = 
