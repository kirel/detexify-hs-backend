module StrokeSample
  (
  newStrokeSample,
  StrokeSample()
  ) where

import Strokes
import DTW
import Classifier

data StrokeSample = StrokeSample {
    strokes :: Strokes
  } deriving (Show)

newStrokeSample :: Strokes -> StrokeSample
newStrokeSample s = StrokeSample s where

instance Sample StrokeSample where
  distance (StrokeSample a) (StrokeSample b) = gdtw manhattanDistance (concat a) (concat b)