module Alpacc.Debug (debug) where

import Debug.Trace (traceShow)

debug :: Show a => a -> a
debug x = traceShow x x