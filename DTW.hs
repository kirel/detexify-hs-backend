module DTW where

import Data.Array

-- using Array (Int,Int) Double as matrix
dtw :: Eq a => ( a -> a -> Double ) -> Int -> [a] -> [a] -> Double
dtw measure w s o = a!(n,m) where
  n = length s
  s' = listArray (1, n) s
  m = length o
  o' = listArray (1, m) o
  a = array ((0,0),(n,m))
        ([((i,j), 1/0) | i <- [0..n], j <- [0..m]] ++
         [((0,0), 0)] ++
         [((i,j), (measure (s'!i) (o'!j)) + minimum [a!(i,j-1), a!(i-1,j-1), a!(i-1,j)] ) | i <- [1..n], j <- [max 1 (i-w)..min m (i+w)]])