module Classifier
  (
  newClassifier,
  trainClassifier,
  classifyWithClassifier,
  Sample(..),
  ) where

import Control.Monad
import Control.Concurrent.STM
import Data.Heap
import Data.List (foldl')
import Data.Map
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
  
data Classifier a = Classifier Int (TVar (Map String [a])) -- Classifier holds Training Data
-- TODO use a different Datastructure than []
-- It should limit on a per sample basis

type Score = Double
type Results = [(String, Score)]

-- helper
update :: TVar a -> (a -> a) -> STM ()
update var f = readTVar var >>= (writeTVar var) . f

-- classifier logic
findKNearestNeighbors :: Sample s => Int -> s -> [s] -> [Hit s]
findKNearestNeighbors k unknown known = Data.Heap.toList $ foldl' step (Data.Heap.empty :: MaxHeap (Hit s)) known where
  step heap next | Data.Heap.size heap < k = Data.Heap.insert (Hit dist next) heap
                 | lb < limit && dist < limit = Data.Heap.insert (Hit dist next) $ fromJust $ viewTail heap
                 | otherwise = heap where
                      lb = distancelb unknown next
                      dist = distance unknown next
                      limit = score $ fromJust $ viewHead heap

alterMin :: Score -> Maybe Score -> Maybe Score
alterMin next Nothing = Just next
alterMin next (Just before) = Just $ min before next
                      
results :: Sample s => [Hit s] -> Results
results hits = Data.Map.toList $ foldl step Data.Map.empty hits where
  step results hit = alter (alterMin $ score hit) (fromJust $ identifier $ sample hit) results
  
-- insert and accumulate samples
insertWithLimit :: Sample s => Int -> s -> Map String [s] -> Map String [s]
insertWithLimit limit sample m = alter alterLim (fromJust $ identifier sample) m where
  alterLim Nothing = Just [sample]
  alterLim (Just l) | length l < limit = Just (sample:l)
                    | otherwise = Just (sample:(init l))

-- classifier interface
newClassifier :: Int -> IO (Classifier s)
newClassifier k = atomically $ liftM (Classifier k) (newTVar Data.Map.empty)

trainClassifier :: Sample s => Classifier s -> s -> IO ()
trainClassifier _ sample | identifier sample == Nothing = error "Can only train samples of known classes."
trainClassifier (Classifier _ t) sample = atomically $ Classifier.update t (insertWithLimit 20 sample)

getSamples :: Sample s => Classifier s -> IO [s]
getSamples (Classifier k t) = do
  m <- atomically $ readTVar t
  return $ fold (++) [] m

classifyWithClassifier :: Sample s => Classifier s -> s -> IO Results
classifyWithClassifier c@(Classifier k t) sample = do
  samples <- getSamples c
  return $ results $ findKNearestNeighbors k sample samples