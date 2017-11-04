module Main where

import Criterion.Main (defaultMainWith, defaultConfig)
import Criterion.Types (Config(..))
import BenchPure
import BenchMap

main :: IO ()
main = defaultMainWith config [ benchPure, benchMap ]
  where config = defaultConfig {reportFile = Just "bench/bench.html"}

