import Network
import qualified Network.Websocket as WS

import Data.Either
import Data.List (sortBy)

import Text.JSON
import Strokes
import Strokes.JSON

import StrokeSample
import Classifier

-- setup classifier

cK = 50
classifier = newClassifier cK

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

-- messages
data Request = TrainReq String Strokes | ClassifyReq Strokes
data Response = TrainRes (Maybe String) | ClassifyRes (Either String Results) | FailedRes

-- JSON stuff
instance JSON Request where
  -- readJSON :: JSValue -> Result Point
  readJSON (JSObject j) = case (rtrain, rid, rclassify) of
    -- train case
    (Ok strokes, Ok id, _) -> Ok (TrainReq id strokes)
    -- classify case
    (_, _, Ok strokes) -> Ok (ClassifyReq strokes)
    otherwise -> Error "Unable to read JSRequest"
    where rtrain = valFromObj "train" j
          rid = valFromObj "id" j
          rclassify = valFromObj "classify" j
  readJSON _ = Error "Unable to read JSRequest"
  -- showJSON :: Request -> JSValue
  -- no need... never encode requests
  showJSON _ = showJSON $ JSNull

jerror s = showJSON $ toJSObject [("error", showJSON s)]

showJSOResults = showJSON . (Prelude.map toJSO) . sortBySnd where -- FIXME sorting could be done on the client...
  toJSO (i, score) = makeObj [("id", showJSON i), ("score", showJSON score)]
  sortBySnd = sortBy cmp where
    cmp a b = compare (snd a) (snd b)

instance JSON Response where
  -- readJSON :: JSValue -> Result Point
  -- no need... never decode requests
  readJSON _ = Error "Unable to read JSResponse"
  -- showJSON :: Point -> JSValue
  showJSON (TrainRes (Just e)) = jerror e
  showJSON (TrainRes (Nothing)) = showJSON $ toJSObject [("message", showJSON "ok")]
  showJSON (ClassifyRes (Left e)) = jerror e
  showJSON (ClassifyRes (Right r)) = showJSON $ toJSObject [("results", showJSOResults r)]  
  showJSON FailedRes = jerror "Illegal request"
  
-- handling requests
-- TODO reject invalid strokes
handle (Ok (TrainReq id strokes)) c = do
  let stroke = process strokes
  trainClassifier c (newStrokeSample stroke (Just id))
  return (TrainRes Nothing)
handle (Ok (ClassifyReq strokes)) c = do
  let stroke = process strokes
  res <- classifyWithClassifier c (newStrokeSample stroke Nothing)
  return (ClassifyRes (Right res))
handle _ c = do
  return FailedRes

-- websockets

onOpen ws = do
  putStrLn "Connection opened"

onClose ws = do
  putStrLn "Connection closed"

onMessage c ws msg = do
    let req = decode msg
    resp <- handle req c
    putStrLn $ "Received message: " ++ msg
    WS.send ws (encode resp)

main = do
  c <- classifier           
  withSocketsDo $ WS.startServer $ WS.Config {
    WS.configPort      = 9876,
    WS.configOrigins   = Nothing,
    WS.configDomains   = Nothing,
    WS.configOnOpen    = onOpen,
    WS.configOnMessage = onMessage c,
    WS.configOnClose   = onClose
  }