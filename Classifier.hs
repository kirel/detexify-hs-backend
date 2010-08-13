module Classifier
  (
  newClassifier,
  trainClassifier,
  classifyWithClassifier,
  getSampleCounts,
  showSamples,
  Sample(..),
  Score(..),
  Results,
  ) where

import Control.Monad
import Control.Concurrent.STM
import Data.Heap
import Data.List (foldl', sortBy)
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
    samplescore :: Double,
    sample :: s
  } deriving (Show)

instance Eq (Hit s) where
  h == o = (samplescore h) == (samplescore o)
  
instance Ord (Hit s) where
  compare h o = compare (samplescore h) (samplescore o)
  
data Classifier a = Classifier Int (TVar (Map String [a])) -- Classifier holds Training Data
-- TODO use a different Datastructure than []
-- It should limit on a per sample basis

showSamples (Classifier _ t) = atomically $ readTVar t

data Score = Score { id :: String, score :: Double } deriving (Show)
type Results = [Score]

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
                      limit = samplescore $ fromJust $ viewHead heap
                      
-- first sort known in lbdist order
-- TODO find out if this should be actually faster...
fastFindKNearestNeighbors :: Sample s => Int -> s -> [s] -> [Hit s]
fastFindKNearestNeighbors k unknown known = Data.Heap.toList $ foldl' step (Data.Heap.empty :: MaxHeap (Hit s)) known' where
  known' = sortBy (\(_,a) (_,b) -> compare a b) $ zip known (Prelude.map (distancelb unknown) known)
  step heap (next, lb) | Data.Heap.size heap < k = Data.Heap.insert (Hit dist next) heap
                       | lb < limit && dist < limit = Data.Heap.insert (Hit dist next) $ fromJust $ viewTail heap
                       | otherwise = heap where
                            limit = samplescore $ fromJust $ viewHead heap
                            dist = distance unknown next

alterMin :: Double -> Maybe Double -> Maybe Double
alterMin next Nothing = Just next
alterMin next (Just before) = Just $ min before next
       
results :: Sample s => [Hit s] -> Results
results hits = Prelude.map toScore $ Data.Map.toList $ foldl step Data.Map.empty hits where
  toScore = uncurry Score
  step results hit = alter (alterMin $ samplescore hit) (fromJust $ identifier $ sample hit) results
  
-- insert and accumulate samples
insertWithLimit :: Sample s => Int -> s -> Map String [s] -> Map String [s]
insertWithLimit limit sample m = alter alterLim (fromJust $ identifier sample) m where
  alterLim Nothing = Just [sample]
  alterLim (Just l) | length l < limit = Just (sample:l)
                    | otherwise = Just (sample:(init l))

-- classifier interface
newClassifier :: Int -> IO (Classifier s)
newClassifier k = atomically $ liftM (Classifier k) (newTVar Data.Map.empty)

-- FIXME sample limit is hardcoded here
trainClassifier :: Sample s => Classifier s -> s -> IO ()
trainClassifier _ sample | identifier sample == Nothing = error "Can only train samples of known classes."
trainClassifier (Classifier _ t) sample = atomically $ Classifier.update t (insertWithLimit 20 sample)

getSamples :: Sample s => Classifier s -> IO [s]
getSamples (Classifier k t) = do
  m <- atomically $ readTVar t
  return $ fold (++) [] m

getSampleCounts :: Sample s => Classifier s -> IO (Map String Int)
getSampleCounts (Classifier k t) = do
  m <- atomically $ readTVar t
  return $ Data.Map.map length m

classifyWithClassifier :: Sample s => Classifier s -> s -> IO Results
classifyWithClassifier c@(Classifier k t) sample = do
  samples <- getSamples c
  return $ results $ findKNearestNeighbors k sample samples
