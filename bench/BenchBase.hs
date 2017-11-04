{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BenchBase (
    benchBase
      , TestableMap(..)
      , bgroup'
      , Benchmark
      , BenchmarkOf
    ) where

import Criterion.Main
import Data.List (foldl')
import Control.DeepSeq

class TestableMap m k v where
    empty :: m k v
    insert :: k -> v -> m k v -> m k v
    fromList :: [(k, v)] -> m k v

newtype BenchmarkOfGround m = BenchmarkOf Benchmark
type BenchmarkOf m = BenchmarkOfGround (m Int Int)

bgroup' :: String -> [BenchmarkOf m] -> Benchmark
bgroup' name = bgroup name . map (\(BenchmarkOf b) -> b)

benchBase :: forall m. (TestableMap m Int Int, NFData (m Int Int)) => BenchmarkOf m
benchBase = BenchmarkOf $
    bgroup "bench base" 
    [
      bench "insert ascending absent" $ nf (insertVals repEven) odds
    , bench "insert ascending present" $ nf (insertVals repEven) evens
    ]
  where
     rep = force . fromList $ zip all all  :: m Int Int
     repEven = force . fromList $ zip evens evens :: m Int Int
     repOdd = force . fromList $ zip odds odds :: m Int Int
     bound = 2^12 :: Int
     all = [1 .. bound `div` 2]
     evens = [0, 2 .. bound]
     odds = [1, 3 .. bound]


     insertVals :: m Int Int -> [Int] -> m Int Int
     insertVals = foldl' (\m k -> insert k k m)

