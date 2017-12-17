{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BenchMap where

import Prelude hiding (lookup)

import Data.Map
import BenchBase

benchMap :: Benchmark
benchMap = bgroup "benchmarking Data.Map.Strict" $
    benchBase (MapOperations {emptyOp = empty, insertOp = insert, lookupOp = lookup} :: MapOperations Map Int Int)

