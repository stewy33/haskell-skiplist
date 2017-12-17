{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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

data MapOperations m k v = MapOperations { emptyOp :: m k v, insertOp :: k -> v -> m k v -> m k v, lookupOp :: k -> m k v -> Maybe v }

benchBase :: NFData (m Int Int) => MapOperations m Int Int -> [Benchmark]
benchBase (MapOperations empty insert lookup) = [
      bench "insert ascending absent" $ nf (insertMany evensMap) odds
    , bench "insert ascending present" $ nf (insertMany evensMap) evens
    , bench "get random" $ nf (lookupMany randsMap) rands
    ]
  where
     fullMap = force $ insertMany empty all
     evensMap = force $ insertMany empty evens
     oddsMap = force $ insertMany empty odds
     randsMap = force . insertMany empty . take bound . randoms $ mkStdGen 353

     bound = 2^12 :: Int

     all = [1 .. bound]
     evens = [0, 2 .. bound * 2]
     odds = [1, 3 .. bound * 2]
     rands = force . take bound . randoms $ mkStdGen 8373

     insertMany = foldl' (\map k -> insert k k map)
     lookupMany map = foldl' (\n k -> fromMaybe n $ lookup k map) 0


