module JSON.Results where
  
import Text.JSON
import Classifier

instance JSON Score where
  -- readJSON :: JSValue -> Result Point
  readJSON (JSObject j) = case (i,s) of
    (Ok i, Ok s) -> Ok (Score i s)
    _ -> Error "Unable to read JSScore"
    where i = valFromObj "id" j
          s = valFromObj "score" j
  readJSON _ = Error "Unable to read JSScore"
  -- showJSON :: Point -> JSValue
  showJSON (Score i s) = showJSON $ toJSObject [("id",showJSON i),("score", showJSON s)]
