module DTW
  (
  dtw, cdtw, gdtw
  ) where

import Data.Array
import Data.List (foldl1')

add :: (Double, Int) -> (Double, Int) -> (Double, Int)
add (a, b) (c, d) = (a+c, b+d)
quo :: (Double, Int) -> Double
quo (a, b) = a/(fromIntegral b)

-- Unconstrained DTW
dtw :: Eq a => ( a -> a -> Double ) -> [a] -> [a] -> Double
dtw measure s [] = gdtw measure [] s
dtw measure [] _ = error "Can not compare empty series!"
dtw measure s o = quo $ a!(n,m) where
  n = length s
  s' = listArray (1, n) s
  m = length o
  o' = listArray (1, m) o
  a = array ((0,0),(n,m))
        ([((i,j), (1/0, 0)) | i <- [0..n], j <- [0..m]] ++
         [((0,0), (0, 0))] ++
         [((i,j), (measure (s'!i) (o'!j), 1) `add` minimum [a!(i,j-1), a!(i-1,j-1), a!(i-1,j)])
            | i <- [1..n], j <- [1..m]])

-- Constrained DTW
cdtw :: Eq a => ( a -> a -> Double ) -> Int -> [a] -> [a] -> Double
cdtw measure w s [] = gdtw measure [] s
cdtw measure w [] _ = error "Can not compare empty series!"
cdtw measure w s o = quo $ a!(n,m) where
  n = length s
  s' = listArray (1, n) s
  m = length o
  o' = listArray (1, m) o
  a = array ((0,0),(n,m))
        ([((i,j), (1/0, 1)) | i <- [0..n], j <- [0..m]] ++
         [((0,0), (0, 1))] ++
         [((i,j), (measure (s'!i) (o'!j), 1) `add` minimum [a!(i,j-1), a!(i-1,j-1), a!(i-1,j)] )
            | i <- [1..n], j <- [max 1 (i-w)..min m (i+w)]])

-- Greedy dtw
gdtw :: Eq a => ( a -> a -> Double ) -> [a] -> [a] -> Double
gdtw measure s [] = gdtw measure [] s
gdtw measure [] _ = error "Can not compare empty series!"
gdtw measure s o = quo $ gdtw' measure s o (measure (head s) (head o), 1) where
  gdtw' measure [a] s (r,l) = (r + foldl1' (+) (map (measure a) s), l + length s)
  gdtw' measure s [a] (r,l) = gdtw' measure [a] s (r,l)
  gdtw' measure s o (r,l) | left   == min = gdtw' measure (tail s) o        (r + left, l+1)
                          | middle == min = gdtw' measure (tail s) (tail o) (r + middle, l+1)
                          | right  == min = gdtw' measure s        (tail o) (r + right, l+1)
                          where
                            left   = measure ((head.tail) s) (head o)
                            middle = measure ((head.tail) s) ((head.tail) o)
                            right  = measure (head s)        ((head.tail) o)
                            min = minimum [left, middle, right]