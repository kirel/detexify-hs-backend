module Classifier
  where

import Control.Monad
import Control.Concurrent.STM

import Strokes

data Sample = Sample String Stroke
idn (Sample sidn str) = sidn
type TrainingData = TVar [Sample]
data Classifier = Classifier TrainingData -- Classifier holds Training Data

-- helper
update :: TVar a -> (a -> a) -> STM ()
update var f = readTVar var >>= (writeTVar var) . f

-- Classifier
newClassifier :: IO Classifier
newClassifier = atomically $ liftM Classifier (newTVar [])

trainClassifier :: Classifier -> String -> Stroke -> IO ()
trainClassifier (Classifier t) idn strokes = atomically $ update t (Sample idn strokes:)

classifyWithClassifier :: Classifier -> Stroke -> IO (String)
classifyWithClassifier (Classifier t) strokes = do
  samples <- atomically $ readTVar t
  let ident = idn (head samples)
  return ident
  
-- testing
main = do
  c <- newClassifier
  trainClassifier c "blub" [(0,0)]
  trainClassifier c "bla" [(0,0)]
  str <- classifyWithClassifier c [(0,0)]
  print str