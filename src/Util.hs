module Util where

import Data.List

allPairs :: [a] -> [(a, a)]
allPairs l = [(x,y) | (x:ys) <- tails l, y <- ys]
