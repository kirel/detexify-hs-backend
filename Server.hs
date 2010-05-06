module Server where

import Network.Loli
import Hack.Handler.SimpleServer
import Hack.Contrib.Request
import Control.Monad.Reader
import Control.Monad.Error
import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy.UTF8 (toString)

import Text.JSON
import Strokes
import Strokes.JSON

import StrokeSample
import Classifier

import Data.Map

cK = 20
classifier = newClassifier cK

preprocess :: Strokes -> Stroke
preprocess = concat

process :: ByteString -> Either String Stroke
process string = do
  strokes <- (resultToEither . decode . toString) string
  return (preprocess strokes)

jsonPair s e = encode $ makeObj [(s, showJSON e)]

jsonError :: String -> String
jsonError = jsonPair "error"

jsonMessage :: String -> String
jsonMessage = jsonPair "message"

-- necessarily ugly :/
jsonResults :: [(String, Double)] -> String
jsonResults = encode . (Prelude.map toJSO) where
  toJSO (i, score) = makeObj [("id", showJSON i), ("score", showJSON score)]

-- server
main = do
  c <- classifier
  (run 3000) . loli $ do
    
  get "/env" $ do
    env <- ask
    (text . show) env
  
  -- real thing
  
  -- classify
  post "/classify" $ do
    env <- ask
    
    j <- liftIO $ either
      (\e -> return $ jsonError e)
      (\stroke -> do
        res <- classifyWithClassifier c $ newStrokeSample stroke Nothing
        return $ jsonResults res)
      (process (body env))
    text $ j

  -- train
  post "/train/:identifier" $ do
    env <- ask
    identifier <- liftM (Prelude.lookup "identifier") captures -- this is always Just
        
    j <- liftIO $ either
      (\e -> return $ jsonError e)
      (\stroke -> do
        trainClassifier c $ newStrokeSample stroke identifier -- weg!    
        return $ jsonMessage "Sample was successfully trained.")
      (process (body env))
    text $ j
    
  -- stats and counts TODO
  get "/" $ do
    s <- liftIO $ showSamples c
    text $ show (s :: Map String [StrokeSample])
    
-- TODO der classifier muss in einen state monat 0_o oder so. So baue ich mir immer nur einen neuen...
