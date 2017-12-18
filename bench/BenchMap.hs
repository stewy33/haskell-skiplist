module BenchMap where

import qualified Data.Map as M
import BenchBase

benchMap :: Benchmark
benchMap = bgroup "benchmarking Data.Map" $
    benchBase (MapOperations {
                  emptyOp  = M.empty
                , insertOp = M.insert
                , lookupOp = M.lookup
                , deleteOp = M.delete
                             } :: MapOperations M.Map Int Int)

