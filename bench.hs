import Data.Time
import Data.Time.Clock.POSIX
import Data.List (sort, sortBy, transpose, nub)
import System.IO
import Random

import Control.Parallel.Strategies
import Control.Parallel

import Strokes
import DTW
import LB
import Classifier

instance Sample StrokeSample where
  distance s t = dtw euclideanDistance 2 (stroke s) (stroke t)

pmap = parMap rwhnf

compute stuff = hPrint stderr stuff

strokeFromList (x:y:rest) = (x,y):(strokeFromList rest)
randomStroke = (strokeFromList . randoms . mkStdGen)

-- main = do
--   let window = 2
--   let strokesize = 30
--   let num = 100
--   let stroke = take strokesize $ (randomStroke 0) :: Stroke
--   let strokes = take num $ (map ((take strokesize) . randomStroke) [1..]) :: [Stroke]
--   putStrLn "Computing hulls..."
--   start <- getPOSIXTime
--   let hulls = (map (hullSeries window) strokes)
--   compute hulls
--   stop <- getPOSIXTime
--   print $ stop - start
--   putStrLn "done."
--   
--   putStrLn "DTW"
--   start <- getPOSIXTime
--   compute (sort (map (dtw euclideanDistance window stroke) strokes))
--   stop <- getPOSIXTime
--   print $ stop - start
--   
--   -- putStrLn "DTWH"
--   -- start <- getPOSIXTime
--   -- compute (sort (map (dtwh euclideanDistance window stroke) strokes))
--   -- stop <- getPOSIXTime
--   -- print $ stop - start
-- 
--   putStrLn "LBDTW"
--   start <- getPOSIXTime
--   -- compute (sort (map (dtwlb stroke) hulls))
--   let mini = (foldl step (1/0) hulls) where -- <--- TODO parallel map of lower bound distances !!!
--             step m hull | (dtwlb stroke hull) < m = min m (dtw euclideanDistance window stroke stroke)
--                         | otherwise = m
--   compute mini
--   stop <- getPOSIXTime
--   print $ stop - start
      
main = do
  c <- newClassifier
  trainClassifier c "blub" [(0,0)]
  trainClassifier c "bla" [(0,0)]
  str <- classifyWithClassifier c [(0,0)]
  print str