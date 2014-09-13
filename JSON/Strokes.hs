{-# LANGUAGE OverloadedStrings #-}
module JSON.Strokes where

import Data.Functor
import Control.Applicative
import Data.Aeson
import Strokes

toPoint a b = Point (a,b)

instance FromJSON Point where
  parseJSON (Object v) = toPoint <$> v .: "x" <*> v .: "y"

instance ToJSON Point where
  toJSON (Point (x,y)) =
    object [ "x" .= x, "y" .= y]
