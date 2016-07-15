{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans
import Control.Monad.Reader

import Data.Maybe
import Data.Either

import Classifier
import StrokeSample
import Strokes
import qualified Data.Aeson as JSON
import JSON.Strokes
import JSON.Results
import Control.Concurrent.STM
import Control.Exception
import System.IO.Error
import System.Environment
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Types (badRequest400)

import Web.Scotty

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
validate :: Either String StrokesRequest -> Either String StrokesRequest
validate (Left s) = Left s
validate (Right (StrokesRequest s)) | ((not.null) s) && (all (not.null) s) = Right (StrokesRequest s)
validate _ = Left "Illegal stroke."

jsonerror :: String -> ActionM ()
jsonerror e = do
  status Network.HTTP.Types.badRequest400
  json $ JSON.object [("error", JSON.toJSON e)]

jsonmessage :: String -> ActionM ()
jsonmessage m = json $ JSON.object [("message", JSON.toJSON m)]

classify :: Classifier StrokeSample -> Either String StrokesRequest -> ActionM ()
classify c d =
  either
    (\e -> do
      jsonerror e)
    (\strokesRequest -> do
      res <- liftIO $ classifyWithClassifier c (newStrokeSample (process (strokes strokesRequest)))
      json $ ResultsResponse $ res)
    d -- comes out as Either String Strokes

train :: Classifier StrokeSample -> Either String StrokesRequest -> String -> ActionM ()
train c d id = either
  (\e -> jsonerror e)
  (\strokeRequest -> do
    let processed = process $ strokes $ strokeRequest
    liftIO $ print $ show processed -- FIXME workaround for strict evaluation
    liftIO $ trainClassifier c id (newStrokeSample processed)
    jsonmessage "Sample was successfully trained.")
  d

main = do
  putStrLn "hs-classifier at http://localhost:3000"
  c <- classifier

  loaded <- loadSuccess c
  case loaded of
    True -> putStrLn "Snapshot loaded."
    False -> putStrLn "No snapshot found."

  scotty port $ do

    get "/" $ do
      json $ JSON.object [("server", "Nöt Betty :("), ("version", "0.0.2")]

    post "/classify" $ do
      d <- body
      let eitherJson = validate $ JSON.eitherDecode $ d
      classify c eitherJson

    post "/train/:id" $ do
      id <- param "id"
      d <- body
      jsonerror "fu"
      let eitherJson = validate $ JSON.eitherDecode $ d
      train c eitherJson id

    -- enableSnapshot <- liftIO $ tryJust (guard . isDoesNotExistError) $ getEnv "ENABLESNAPSHOT"
    -- when (isRight enableSnapshot) $ do
    --
    --   post "/save-snapshot" $ do
    --     liftIO $ snapshot c
    --     jsonmessage "Snapshotted."
    --
    --   post "/load-snapshot" $ do
    --     success <- liftIO $ loadSuccess c
    --     case success of
    --       True -> jsonmessage "Loaded"
    --       False -> jsonmessage "No. Just no."

snapshotJsonFile = "snapshot.json"

snapshot :: Classifier StrokeSample -> IO ()
snapshot c = do
  snapshot <- atomically $ (readTVar $ samples c)
  BL.writeFile snapshotJsonFile $ JSON.encode snapshot

load :: Classifier StrokeSample -> IO ()
load c = do
  jsonString <- BL.readFile snapshotJsonFile
  case (JSON.decode jsonString) of
    Nothing -> return ()
    Just snapshot -> atomically $ (writeTVar (samples c) snapshot)

loadSuccess c = load c >> return True `catch` handle where
  handle :: IOError -> IO Bool
  handle e
    | isDoesNotExistError e = return False
    | otherwise = ioError e
