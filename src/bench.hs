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

strokeFromList (x:y:rest) = (Point (x,y)):(strokeFromList rest)
randomStroke :: Int -> Stroke
randomStroke = (strokeFromList . randoms . mkStdGen)

cK = 1
strokesize = 30
num = 20000
stroke = take strokesize $ (randomStroke 0)
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