-- Hgrep.hs
--
--     Author: Fabian Meyer
-- Created on: 29 Jan 2018

module Hgrep (run) where

import qualified Hgrep.Internal as H

run :: IO ()
run = H.run
