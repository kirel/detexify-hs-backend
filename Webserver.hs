module Main where

import Hack.Handler.Happstack
import qualified Hack.Contrib.Request as Request
import qualified Hack.Contrib.Response as Response

import Network.Loli hiding(mime)
import Network.Loli.Utils
import Network.Loli.Type

import Control.Monad.Trans
import Control.Monad.Reader

import Data.ByteString.Lazy.UTF8 (fromString, toString)

import Classifier
import StrokeSample
import Strokes
import Text.JSON
import JSON.Strokes
import JSON.Results

-- import Data.List(sortBy)

cK = 50
classifier = newClassifier cK

-- ord :: Stroke -> Stroke -> Ordering
-- ord s t = compare ((angle.last) s) ((angle.last) t) where
--   angle (Point (x,y)) = atan ((1-y)/x)
-- 
-- sort = sortBy ord

sanitize :: Strokes -> Strokes
sanitize = (map (unduplicate.redistribute 10.refit (0,0,1,1).smooth.unduplicate)).limit 10

process :: Strokes -> Strokes
process = sanitize

-- validate there are no empty strokes
validate :: Either String Strokes -> Either String Strokes
validate (Left s) = Left s
validate (Right s) | ((not.null) s) && (all (not.null) s) = Right s
validate _ = Left "Illegal stroke."

-- in/out

status s = update $ Response.set_status s
mime m = update $ Response.set_content_type m
body b = update $ Response.set_body (fromString b)

reqBody = do
  env <- ask
  return $ toString $ Request.body env

json :: JSON j => j -> AppUnit
json = \d -> mime "application/json" >> (body . encode) d
jsonerror e = do
  status 400
  json $ toJSObject [("error", e)]
jsonmessage m = json $ toJSObject [("message", m)]

serverinfo = json $ toJSObject [("server", "Not Betty :("), ("version", "0.0.1")]

classify c d =
  either
    (\e -> jsonerror e)
    (\strokes -> do
      res <- liftIO $ classifyWithClassifier c (newStrokeSample (process strokes) Nothing)
      json res)
    (validate $ resultToEither $ decode $ d) -- comes out as Either String Strokes

train _ _ Nothing = jsonerror "no training without an id"
train c d id = either
  (\e -> jsonerror e)
  (\strokes -> do
    liftIO $ trainClassifier c (newStrokeSample (process strokes) id)
    jsonmessage "Sample was successfully trained.")
  (validate $ resultToEither $ decode $ d)
  
main = do
  putStrLn "hs-classifier at http://localhost:3000"

  c <- classifier

  run . loli $ do
    
    get "/" $ do
      serverinfo
    
    post "/classify" $ do
      d <- reqBody
      classify c d
    
    post "/train/:id" $ do
      id <- liftM (Prelude.lookup "id") captures
      d <- reqBody
      train c d id