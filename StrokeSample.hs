module StrokeSample
  (
  newStrokeSample,
  StrokeSample()
  ) where

import Strokes
import DTW
import LB
import Classifier

_window_ = 2

data StrokeSample = StrokeSample {
    stroke :: Stroke,
    hullSeries :: [ConvexHull],
    windowWidth :: Int,
    sidentifier :: Maybe String
  } deriving (Show)

newStrokeSample :: Stroke -> Maybe String -> StrokeSample
newStrokeSample s i = StrokeSample s hulls _window_ i where
  hulls = (LB.hullSeries _window_ s)

instance Sample StrokeSample where
  distance (StrokeSample _ _ w _) (StrokeSample _ _ v _) | w /= v = error "Dimension mismatch."
  distance (StrokeSample a _ _ _) (StrokeSample b _ _ _) = dtw euclideanDistance _window_ a b
  distancelb (StrokeSample _ _ w _) (StrokeSample _ _ v _) | w /= v = error "Dimension mismatch."
  distancelb (StrokeSample _ b _ _) (StrokeSample a _ _ _) = 0--dtwlb a b
  identifier = sidentifier  