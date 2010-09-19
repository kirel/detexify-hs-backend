module StrokeSample
  (
  newStrokeSample,
  StrokeSample()
  ) where

import Strokes
import DTW
import Classifier

data StrokeSample = StrokeSample {
    strokes :: Strokes,
    sidentifier :: Maybe String
  } deriving (Show)

newStrokeSample :: Strokes -> Maybe String -> StrokeSample
newStrokeSample s i = StrokeSample s i where

instance Sample StrokeSample where
  distance (StrokeSample a _) (StrokeSample b _) = gdtw manhattanDistance (concat a) (concat b)
  identifier = sidentifier
