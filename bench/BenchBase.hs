module BenchBase (
    benchBase
    , Benchmark
    , bgroup
    , MapOperations(..)
    ) where

import Control.DeepSeq
import Criterion.Main
import Data.List (foldl')
import Data.Maybe (Maybe, fromMaybe)
import System.Random

data MapOperations m k v = MapOperations {
                              emptyOp :: m k v
                            , insertOp :: k -> v -> m k v -> m k v
                            , lookupOp :: k -> m k v -> Maybe v
                            , deleteOp :: k -> m k v -> m k v
                                         }

-- polymorphic benchmarking function that takes a record to define map
-- operations
benchBase :: NFData (m Int Int) => MapOperations m Int Int -> [Benchmark]
benchBase (MapOperations empty insert lookup delete) = [
      bench "insert ascending absent" $ nf (insertMany evensMap) odds
    , bench "insert ascending present" $ nf (insertMany evensMap) evens
    , bench "insert randoms from empty" $ nf (insertMany empty) rands
    , bench "lookup ascending absent" $ nf (lookupMany evensMap) odds
    , bench "lookup ascending present" $ nf (lookupMany evensMap) evens
    , bench "lookup random" $ nf (lookupMany randsMap) rands
    , bench "delete ascending absent" $ nf (deleteMany evensMap) odds
    , bench "delete ascending present" $ nf (deleteMany evensMap) evens
    , bench "delete random" $ nf (deleteMany randsMap) rands
    ]
  where
     evensMap = force $ insertMany empty evens
     randsMap = force $ insertMany empty rands2

     bound = 2^12 :: Int

     evens = [0, 2 .. bound * 2]
     odds = [1, 3 .. bound * 2]
     rands = force . take bound . randoms $ mkStdGen 8373
     rands2 = force . take bound . randoms $ mkStdGen 353

     insertMany = foldl' (\map k -> insert k k map)
     -- this fromMaybe fold forces evaluation
     lookupMany map = foldl' (\n k -> fromMaybe n $ lookup k map) 0
     deleteMany = foldl' (\map k -> delete k map)


