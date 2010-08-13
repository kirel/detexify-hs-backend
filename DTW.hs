module DTW
  (
  dtw, udtw
  ) where

import Data.Array

add (a, b) (c, d) = (a+c, b+d)
quo (a, b) = a/b

-- using Array (Int,Int) (Double, Int) as matrix
-- Double tracks warping distance
-- Int tracks warping path length
-- TODO check dimensions
dtw :: Eq a => ( a -> a -> Double ) -> Int -> [a] -> [a] -> Double
dtw measure w s o = quo $ a!(n,m) where
  n = length s
  s' = listArray (1, n) s
  m = length o
  o' = listArray (1, m) o
  a = array ((0,0),(n,m))
        ([((i,j), (1/0, 1)) | i <- [0..n], j <- [0..m]] ++
         [((0,0), (0, 1))] ++
         [((i,j), (measure (s'!i) (o'!j), 1) `add` minimum [a!(i,j-1), a!(i-1,j-1), a!(i-1,j)] )
            | i <- [1..n], j <- [max 1 (i-w)..min m (i+w)]])
            
udtw :: Eq a => ( a -> a -> Double ) -> [a] -> [a] -> Double
udtw measure s o = quo $ a!(n,m) where
  n = length s
  s' = listArray (1, n) s
  m = length o
  o' = listArray (1, m) o
  a = array ((0,0),(n,m))
        ([((i,j), (1/0, 1)) | i <- [0..n], j <- [0..m]] ++
         [((0,0), (0, 1))] ++
         [((i,j), (measure (s'!i) (o'!j), 1) `add` minimum [a!(i,j-1), a!(i-1,j-1), a!(i-1,j)] )
            | i <- [1..n], j <- [1..m]])