{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BenchPure where

import BenchBase
import qualified Data.SkipList.Pure as SKP

instance Ord k => TestableMap SKP.SkipList k v where
    empty = SKP.empty $ SKP.mkStdGen 0
    insert = SKP.insert
    fromList = SKP.fromList $ SKP.mkStdGen 0

benchPure :: Benchmark
benchPure = bgroup' "benchmarking Data.SkipList.Pure"
    [benchBase :: BenchmarkOf SKP.SkipList]

