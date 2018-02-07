-- Spec.hs
--
--     Author: Fabian Meyer
-- Created on: 07 Feb 2018

import qualified RegexSpec as RS

import Test.HUnit

main :: IO ()
main = do
    runTestTT (TestList $ RS.tests)
    return ()
