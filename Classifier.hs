module Classifier
  (
  newClassifier,
  trainClassifier,
  classifyWithClassifier
  ) where

import Control.Monad
import Control.Concurrent.STM
import Data.Heap
import Data.Maybe

import Strokes
import DTW

class Sample a where
  distance :: a -> a -> Double
  distancelb :: a -> a -> Double
  distancelb _ _ = 0.0
  identifier :: a -> Maybe String -- Nothing for unknown class

data Hit s = Hit {
    score :: Double,
    sample :: s
  } deriving (Show)

instance Eq (Hit s) where
  h == o = (score h) == (score o)
  
instance Ord (Hit s) where
  compare h o = compare (score h) (score o)
  
data Classifier a = Classifier (TVar [a]) -- Classifier holds Training Data

-- helper
update :: TVar a -> (a -> a) -> STM ()
update var f = readTVar var >>= (writeTVar var) . f

-- classifier logic
findKNearestNeighbors :: Sample s => Int -> s -> [s] -> [Hit s]
findKNearestNeighbors k unknown known = toList $ foldl step (empty :: MaxHeap (Hit s)) known where
  step heap next | size heap < k = insert (Hit dist next) heap
                 | (lb < limit) && (dist < limit) = insert (Hit dist next) $ fromJust $ viewTail heap where
                      lb = distancelb unknown next
                      dist = distance unknown next
                      limit = score $ fromJust $ viewHead heap

-- classifier interface
newClassifier :: IO (Classifier s)
newClassifier = atomically $ liftM Classifier (newTVar [])

trainClassifier :: Sample s => Classifier s -> s -> IO ()
trainClassifier _ sample | identifier sample == Nothing = error "Can only train samples of known classes."
trainClassifier (Classifier t) sample = atomically $ update t (sample:)

classifyWithClassifier :: Sample s => Classifier s -> s -> IO [Hit s]
classifyWithClassifier (Classifier t) sample = do
  samples <- atomically $ readTVar t
  -- work with samples in a pure way here!
  let Just ident = identifier (head samples) -- 
  return [Hit 0.0 sample]