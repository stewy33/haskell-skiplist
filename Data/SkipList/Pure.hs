module Data.SkipList.Pure (
    SkipList()
      , member
      , empty
      , singleton
      , insert
      , adjust
      , lookup
      , delete
      , fromList
      , toList
      , mkStdGen
    ) where

import Prelude hiding (lookup)

import Data.SkipList.Pure.Internal
import System.Random

