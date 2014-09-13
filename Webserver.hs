{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans
import Control.Monad.Reader

import Data.Maybe

import Classifier
import StrokeSample
import Strokes
import Data.Aeson (toJSON, object, eitherDecode)
import JSON.Strokes
import JSON.Results

import Network.HTTP.Types (badRequest400)


import Web.Scotty

-- import Data.List(sortBy)

port = 3000

cK = 50
classifier = newClassifier cK

alpha = 2*pi*15/360

sanitize :: Strokes -> Strokes
sanitize = (map (dominant alpha
                .unduplicate
                .redistribute 10
                .aspectrefit (Point (0,0), Point (1,1))
                .smooth
                .unduplicate)
                ).limit 10

process :: Strokes -> Strokes
process = sanitize

-- validate there are no empty strokes
validate :: Either String Strokes -> Either String Strokes
validate (Left s) = Left s
validate (Right s) | ((not.null) s) && (all (not.null) s) = Right s
validate _ = Left "Illegal stroke."

jsonerror :: String -> ActionM ()
jsonerror e = do
  status Network.HTTP.Types.badRequest400
  json $ object [("error", toJSON e)]

jsonmessage :: String -> ActionM ()
jsonmessage m = json $ object [("message", toJSON m)]

classify :: Classifier StrokeSample -> Either String Strokes -> ActionM ()
classify c d =
  either
    (\e -> do
      jsonerror e)
    (\strokes -> do
      res <- liftIO $ classifyWithClassifier c (newStrokeSample (process strokes))
      json res)
    d -- comes out as Either String Strokes

train :: Classifier StrokeSample -> Either String Strokes -> String -> ActionM ()
train c d id = either
  (\e -> jsonerror e)
  (\strokes -> do
    let processed = (process strokes)
    liftIO $ print $ show processed -- FIXME workaround for strict evaluation
    liftIO $ trainClassifier c id (newStrokeSample processed)
    jsonmessage "Sample was successfully trained.")
  d

main = do
  putStrLn "hs-classifier at http://localhost:3000"
  c <- classifier
  scotty port $ do

    get (capture "/") $ do
      json $ object [("server", "Nöt Betty :("), ("version", "0.0.2")]

    post (capture "/classify") $ do
      d <- body
      let eitherJson = validate $ eitherDecode $ d
      classify c eitherJson

    post (capture "/train/:id") $ do
      id <- param "id"
      d <- body
      jsonerror "fu"
      let eitherJson = validate $ eitherDecode $ d
      train c eitherJson id
