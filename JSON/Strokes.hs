module JSON.Strokes where
  
import Text.JSON
import Strokes

instance JSON Point where
  -- readJSON :: JSValue -> Result Point
  readJSON (JSObject j) = case (rx,ry) of
    (Ok x, Ok y) -> Ok (Point (x,y))
    _ -> Error "Unable to read JSPoint"
    where rx = valFromObj "x" j
          ry = valFromObj "y" j
  readJSON _ = Error "Unable to read JSPoint"
  -- showJSON :: Point -> JSValue
  showJSON (Point (x,y)) = showJSON $ toJSObject [("x",x),("y",y)]

-- main = do
--   let rp = (decode "[[{\"x\":1,\"y\":3}]]") :: Result Strokes
--   putStrLn $ show rp
--   case rp of
--     Ok p -> putStrLn $ encode $ p
--     Error e -> putStrLn e
