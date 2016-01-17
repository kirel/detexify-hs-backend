{-# LANGUAGE OverloadedStrings #-}
module JSON.Results where

import Data.Functor
import Control.Applicative
import Data.Aeson
import Classifier

data ResultsResponse = ResultsResponse { results :: Results }

instance FromJSON ResultsResponse where
  parseJSON (Object v) = ResultsResponse <$> v .: "results"
instance ToJSON ResultsResponse where
  toJSON (ResultsResponse r) = object [ "results" .= r ]

instance FromJSON Score where
  parseJSON (Object v) = Score <$> v .: "id" <*> v .: "score"
instance ToJSON Score where
  toJSON (Score id score) =
    object ["id" .= id, "score" .= score]
