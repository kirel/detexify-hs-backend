module Classifier
  (
  newClassifier,
  trainClassifier,
  classifyWithClassifier,
  Sample(..),
  Score(..),
  Results,
  ) where

import Control.Monad
import Control.Concurrent.STM
import Control.Parallel.Strategies
import Control.Parallel
-- import Data.Heap
import Data.List (foldl', sortBy, sort)
import qualified Data.Map as Hash
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
  
data Classifier a = Classifier {
  samplelimit :: Int,
  samples :: TVar (Hash.Map String [a])
} -- Classifier holds Training Data


data Score = Score { id :: String, score :: Double } deriving (Show)
type Results = [Score]

pmap = (parMap rwhnf)

-- helper
updateTVar :: TVar a -> (a -> a) -> STM ()
updateTVar var f = readTVar var >>= (writeTVar var) . f

-- classifier logic
-- findKNearestNeighbors :: Sample s => Int -> s -> [s] -> [Hit s]
-- findKNearestNeighbors k unknown known = Data.Heap.toList $ foldl' step (Data.Heap.empty :: MaxHeap (Hit s)) known where
--   step heap next | Data.Heap.size heap < k = Data.Heap.insert (Hit dist next) heap
--                  | lb < limit && dist < limit = Data.Heap.insert (Hit dist next) $ fromJust $ viewTail heap
--                  | otherwise = heap where
--                       lb = distancelb unknown next
--                       dist = distance unknown next
--                       limit = samplescore $ fromJust $ viewHead heap
--                       
-- alterMin :: Double -> Maybe Double -> Maybe Double
-- alterMin next Nothing = Just next
-- alterMin next (Just before) = Just $ min before next
--        
-- results :: Sample s => [Hit s] -> Results
-- results hits = Prelude.map toScore $ Hash.toList $ foldl step Hash.empty hits where
--   toScore = uncurry Score
--   step results hit = Hash.alter (alterMin $ samplescore hit) (fromJust $ identifier $ sample hit) results
  
-- insert and accumulate samples
insertWithLimit :: Sample s => Int -> s -> Hash.Map String [s] -> Hash.Map String [s]
insertWithLimit limit sample m = Hash.alter alterLim (fromJust $ identifier sample) m where
  alterLim Nothing = Just [sample]
  alterLim (Just l) | length l < limit = Just (sample:l)
                    | otherwise = Just (sample:(init l))

-- classifier interface
newClassifier :: Int -> IO (Classifier s)
newClassifier k = atomically $ liftM (Classifier k) (newTVar Hash.empty)

trainClassifier :: Sample s => Classifier s -> s -> IO ()
trainClassifier _ sample | identifier sample == Nothing = error "Can only train samples of known classes."
trainClassifier (Classifier k t) sample = atomically $ updateTVar t (insertWithLimit k sample)

getSamples (Classifier _ t) = atomically $ readTVar t

classifyWithClassifier :: Sample s => Classifier s -> s -> IO Results
classifyWithClassifier c@(Classifier k t) unknown = do
  samples <- getSamples c
  return $ map toScore $ sort $ pmap bestHit $ Hash.elems $ samples where
    toScore hit = Score ((fromJust.identifier.sample) hit) (samplescore hit)
    bestHit = minimum . (map (\next -> Hit (distance unknown next) next))
    -- bestHit = avgminimum2 . (map (\next -> Hit (distance unknown next) next))
    avgminimum2 unsorted = merge m m' m'' where
      m = head sorted
      m' = (head.tail) sorted
      m'' = (head.tail.tail) sorted
      sorted = sort unsorted
      merge (Hit d1 s) (Hit d2 _) (Hit d3 _) = Hit ((d1+d2+d3)/3) s
