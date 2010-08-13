module Main where

import qualified Hack as Hack
import Hack.Handler.Happstack
import Bird
import qualified Bird as Bird
import Bird.Translator.Hack
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import Control.Monad.Trans

import Prelude hiding( log )

import Classifier
import StrokeSample
import Strokes
import Text.JSON
import JSON.Strokes
import JSON.Results

-- logic

cK = 10
classifier = newClassifier cK

sanitize :: Strokes -> Strokes
sanitize = cleanStrokes . removeEmptyStrokes . (limitStrokes 10)

process :: Strokes -> Stroke
process = concat . (multiredistribute 30) . (refitStrokes (0,0,1,1)) . sanitize

-- in/out

json :: JSON j => j -> Handler
json = \d -> mime "application/json" >> (body . encode) d
jsonerror e = do
  status 422
  json $ toJSObject [("error", e)]
jsonmessage m = json $ toJSObject [("message", m)]

serverinfo = json $ toJSObject [("server", "Betty, the smart crow"), ("version", "0.0.1")]

classify c d =
  either
    (\e -> jsonerror e)
    (\strokes -> do
      res <- liftIO $ classifyWithClassifier c (newStrokeSample (process strokes) Nothing)
      log $ show $ res
      json res)
    (resultToEither $ decode $ d) -- comes out as Either String Strokes

train _ _ Nothing = json "bla"
train c d id = either
  (\e -> jsonerror e)
  (\strokes -> do
    liftIO $ trainClassifier c (newStrokeSample (process strokes) id)
    jsonmessage "Sample was successfully trained.")
  (resultToEither $ decode $ d)

main = do
  putStrLn "Betty, the smart crow, was just spotted in flight at http://localhost:3000"
  
  c <- classifier

  run $ bird (router c) where
    router c = do
      r <- request
      case verb r of 
        Bird.GET -> get c $ path r
        Bird.POST -> post c $ path r
        Bird.PUT -> put c $ path r
        Bird.DELETE -> delete c $ path r

    get c [] = serverinfo
    get    _ _ = status 404
    post _ ["env"] = body.show =<< R.ask
    post c ["classify"] = do
      d <- reqBody
      classify c d 
    post c ["train", id] = do
      d <- reqBody
      train c d (Just id)
    post   _ _ = status 404
    put    _ _ = status 404
    delete _ _ = status 404
