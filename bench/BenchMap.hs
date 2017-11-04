{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BenchMap where

import qualified Data.Map.Strict as M
import BenchBase

instance Ord k => TestableMap M.Map k v where
    empty = M.empty
    insert = M.insert
    fromList = M.fromList

benchMap :: Benchmark
benchMap = bgroup' "benchmarking Data.Map.Strict"
    [benchBase :: BenchmarkOf M.Map]

