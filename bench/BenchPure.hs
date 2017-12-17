{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BenchPure where

import BenchBase
import qualified Data.SkipList.Pure as SKP

benchPure :: Benchmark
benchPure = bgroup "benchmarking Data.SkipList.Pure" $
    benchBase (MapOperations { emptyOp = SKP.empty $ SKP.mkStdGen 0
                             , insertOp = SKP.insert
                             , lookupOp = SKP.lookup }
                                 :: MapOperations SKP.SkipList Int Int)
