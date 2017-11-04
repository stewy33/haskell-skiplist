module Data.SkipList.Pure (
    SkipList()
      , member
      , empty
      , mkStdGen
      , singleton
      , insert
      , adjust
      , delete
      , fromList
      , toList
      , mkStdGen
    ) where

import Data.SkipList.Pure.Internal
import System.Random

