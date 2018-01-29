-- Hgrep.Internal.hs
--
--     Author: Fabian Meyer
-- Created on: 29 Jan 2018

module Hgrep.Internal where

import Options.Applicative
import Data.Semigroup ((<>))
import Regex

data Opts = Opts {
        nocolor :: Bool,
        pattern :: String,
        files :: [String]}

optParser :: Parser Opts
optParser = Opts
    <$> switch
        (long "no-color"
         <> help "do not use colors")
    <*> argument str (metavar "PATTERN")
    <*> some
        (argument str (metavar "[FILE]..."))

run :: IO ()
run = runSearch =<< execParser opts
    where opts = info (optParser <**> helper)
                 (fullDesc
                  <> progDesc "Search for PATTERN in each FILE.")

runSearch :: Opts -> IO ()
runSearch o =
    case compile (pattern o) of
        Nothing -> fail "Inval pattern"
        Just r  -> mapM_ (matchFile r) (files o)

matchFile :: Regex -> String -> IO ()
matchFile r f = do
    input <- readFile f
    mapM_ (matchLine r) (lines input)

matchLine :: Regex -> String -> IO ()
matchLine r l = case matchAll r l of
        [] -> return ()
        ms -> printStr . foldr g (0,"") $ ms
    where g (n, m) (con,s) = (con + (length m) + ((length l)-n-(length m)-con),
                             (colorStr m) ++ (substr (n+length m) ((length l)-n-(length m)-con) l) ++ s)
          printStr (con, s) = putStrLn ((take (length l - con) l) ++ s)


substr :: Int -> Int -> String -> String
substr b l = take l . drop b

colorStr :: String -> String
colorStr s = "*" ++ s ++ "*"
