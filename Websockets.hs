import Network
import qualified Network.Websocket as WS

config = WS.Config {
           WS.configPort      = 9876,
           WS.configOrigins   = Nothing,
           WS.configDomains   = Nothing,
           WS.configOnOpen    = onOpen,
           WS.configOnMessage = onMessage,
           WS.configOnClose   = onClose
         }

main = withSocketsDo $ WS.startServer config
         

onOpen ws = do
  putStrLn "Connection opened"

onMessage ws msg = do
  putStrLn $ "Received message: " ++ msg
  WS.send ws msg

onClose ws = do
  putStrLn "Connection closed"