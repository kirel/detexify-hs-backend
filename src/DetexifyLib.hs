{-# LANGUAGE ForeignFunctionInterface #-}

module DetexifyLib where

import Classifier
import Strokes
import StrokeSample
import qualified Data.Aeson as JSON
import JSON.Results
import JSON.Strokes
import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Foreign.C

import System.IO.Unsafe
import System.Directory

--

cK = 50
snapshotJsonFileRef :: IORef String
snapshotJsonFileRef = unsafePerformIO (newIORef "snapshot.json")
{-# NOINLINE snapshotJsonFileRef #-}

load :: IO (Classifier StrokeSample)
load = do
  snapshotJsonFile <- readIORef snapshotJsonFileRef
  putStrLn snapshotJsonFile
  jsonString <- BL.readFile snapshotJsonFile
  case (JSON.decode jsonString) of
    Nothing -> error "could not read snapshot"
    Just snapshot -> newClassifierWithSnapshot cK snapshot
      -- s <- getSamples c
      -- putStrLn $ show $ length s

classifier :: Classifier StrokeSample
classifier = unsafePerformIO load
{-# NOINLINE classifier #-}

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

-- classify

classify :: StrokesRequest -> IO Results
classify strokesreq = do
  -- putStrLn $ show $ strokes strokesreq
  -- putStrLn $ show $ process $ strokes strokesreq
  -- putStrLn $ show $ newStrokeSample $ process $ strokes strokesreq
  samples <- getSamples classifier
  -- putStrLn $ show $ length samples
  res <- classifyWithClassifier classifier (newStrokeSample (process (strokes strokesreq)))
  -- putStrLn $ show $ length res
  return $ res

foreign export ccall classify_export :: CString -> IO CString
classify_export :: CString -> IO CString
classify_export cstring = do
  json <- B.packCString cstring
  let strokesreq = JSON.decode $ BL.fromStrict json
  res <- case strokesreq of
          Nothing -> return []
          (Just strokesreq) -> classify strokesreq
  let bstring = JSON.encode $ ResultsResponse res
  returncstring <- newCString $ BL.unpack $ bstring
  return returncstring

foreign export ccall init_detexify :: CString -> IO ()
init_detexify :: CString -> IO ()
init_detexify newJsonPathCString = do
  newPath <- peekCString newJsonPathCString
  writeIORef snapshotJsonFileRef newPath
  getSamples classifier >> return ()
