module Classifier
  (
  Classifier,
  samples,
  newClassifier,
  newClassifierWithSnapshot,
  trainClassifier,
  classifyWithClassifier,
  getSamples,
  Sample(..),
  Score(..),
  Results,
  ) where

import Control.Monad
import Control.Concurrent.STM
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
  
data Hit s = Hit {
    samplescore :: Double,
    sample :: s
  } deriving (Show)

data Score = Score { identifier :: String, score :: Double } deriving (Show)
type Results = [Score]

instance Eq Score where
  h == o = (score h) == (score o)
  
instance Ord Score where
  compare h o = compare (score h) (score o)
  
data Classifier a = Classifier {
  samplelimit :: Int,
  samples :: TVar (Hash.Map String [a])
} -- Classifier holds Training Data

-- helper
updateTVar :: TVar a -> (a -> a) -> STM ()
updateTVar var f = readTVar var >>= (writeTVar var) . f
  
-- insert and accumulate samples
-- TODO find besser data structure than list for cheaper insertion when full
insertWithLimit :: Sample s => Int -> String -> s -> Hash.Map String [s] -> Hash.Map String [s]
insertWithLimit limit identifier sample m = Hash.alter alterLim identifier m where
  alterLim Nothing = Just [sample]
  alterLim (Just l) | length l < limit = Just (sample:l)
                    | otherwise = Just (sample:(init l))

-- classifier interface
newClassifier :: Int -> IO (Classifier s)
newClassifier k = atomically $ liftM (Classifier k) (newTVar Hash.empty)

newClassifierWithSnapshot :: Int -> Hash.Map String [s] -> IO (Classifier s)
newClassifierWithSnapshot k s = atomically $ liftM (Classifier k) (newTVar s)

trainClassifier :: Sample s => Classifier s -> String -> s -> IO ()
trainClassifier (Classifier k t) identifier sample = atomically $ updateTVar t (insertWithLimit k identifier sample)

getSamples (Classifier _ t) = atomically $ readTVar t

classifyWithClassifier :: Sample s => Classifier s -> s -> IO Results
classifyWithClassifier c@(Classifier k t) unknown = do
  samples <- getSamples c
  return $ (sort . toScores . Hash.toList . Hash.map mindist) samples where
    toScores = map (\(id, score) -> Score id score)
    mindist = (meanmin 2) . (map (\next -> distance unknown next))
    meanmin n unsorted = mean (take n sorted) where
      sorted = sort unsorted
      mean = (\(a,b) -> (a/b)) . (foldl (\(a,b) c -> (a+c,b+1)) (0,0))
