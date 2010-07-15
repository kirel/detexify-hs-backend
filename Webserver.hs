module Main where

import Network.Loli
import Network.Loli.Utils
import Network.Loli.Template.TextTemplate
import Hack.Handler.SimpleServer
import Hack.Contrib.Request
import Hack.Contrib.Response
import Control.Monad.Reader
import Control.Monad.Error
import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy.UTF8 (toString, fromString)

import Text.JSON
import Strokes
import Strokes.JSON

import StrokeSample
import Classifier

import Data.Map hiding (update)
import Data.List (sortBy)

cK = 20
classifier = newClassifier cK

sanitize :: Strokes -> Strokes
sanitize = cleanStrokes . removeEmptyStrokes . (limitStrokes 10)

preprocess :: Strokes -> Stroke
preprocess = concat . (multiredistribute 30) . sanitize

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
jsonResults = encode . (Prelude.map toJSO) . sortBySnd where -- FIXME sorting could be done on the client...
  toJSO (i, score) = makeObj [("id", showJSON i), ("score", showJSON score)]
  sortBySnd = sortBy cmp where
    cmp a b = compare (snd a) (snd b)

-- server
main = do
  c <- classifier
  print "Server starting at port 3000"
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
    update $ set_content_type "application/json"
    update $ set_body (fromString j)
    -- output $ text_template j

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
    update $ set_content_type "application/json"
    update $ set_body (fromString j)
    -- output $ text_template j
    
  -- stats and counts TODO
  get "/" $ do
    s <- liftIO $ getSampleCounts c
    let j = encode $ makeObj [("counts", showJSON (toJSObject (toList s)))]
    update $ set_content_type "application/json"
    update $ set_body (fromString j)

  get "/samples" $ do
    s <- liftIO $ showSamples c
    text $ show (s :: Map String [StrokeSample])
    
-- TODO der classifier muss in einen state monat 0_o oder so. So baue ich mir immer nur einen neuen...
