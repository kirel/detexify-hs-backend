import Data.Time
import Data.Time.Clock.POSIX
import Data.List (sort, sortBy, transpose, nub)
import System.IO
import Random

import Control.Parallel.Strategies
import Control.Parallel
import Control.Monad

import Strokes
import StrokeSample
import Classifier

pmap = parMap rwhnf

compute stuff = hPrint stderr stuff

strokeFromList (x:y:rest) = (x,y):(strokeFromList rest)
randomStroke :: Int -> Stroke
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

cK = 10
strokesize = 30
num = 20000
stroke = take strokesize $ (randomStroke 1)
strokes = take num $ (map ((take strokesize) . randomStroke) [1..])
ids = cycle $ map show [1..1000]

main = do
  c <- newClassifier cK
  -- train with samples
  print "Training the classifier..."
  start <- getPOSIXTime
  forM_ (zip strokes ids) $ \(s, ident) -> do
    trainClassifier c $ newStrokeSample s (Just ident)
  stop <- getPOSIXTime
  print $ stop - start

  print "Classifying..."
  start <- getPOSIXTime
  results <- classifyWithClassifier c $ newStrokeSample stroke Nothing
  compute results
  stop <- getPOSIXTime
  print $ stop - start
  
  print "Classifying again..."
  start <- getPOSIXTime
  results <- classifyWithClassifier c $ newStrokeSample stroke Nothing
  compute results
  stop <- getPOSIXTime
  print $ stop - start
  
  print "And again..."
  start <- getPOSIXTime
  results <- classifyWithClassifier c $ newStrokeSample stroke Nothing
  compute results
  stop <- getPOSIXTime
  print $ stop - start
  
  print $ results