module Main where

import Criterion.Main
import Data.SkipList.Pure

newSkp :: SkipList k v
newSkp = empty $ mkStdGen 5

insertVals :: Int -> 
insertVals x = foldr (\x skp -> insert x x skp) newSkp [1..x]

benchInsert = bGroup "insert"
    [ bench "1..5" $ nf insertVals 5
      , bench "1.10" $ nf insertVals 10
    ]


main = defaultMain [ benchInsert ]
