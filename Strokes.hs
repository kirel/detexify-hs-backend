module Strokes
  (
  Point, Stroke, Strokes, Points,
  add, sub, dot, scalar, norm,
  euclideanDistance, manhattanDistance
  ) where

-- data types
type Point = (Double, Double)
type Stroke = [Point]
type Strokes = [Stroke]
type Points = [Point]

-- elementwise = zipWith for Points
elementwise :: (Double -> Double) -> Point -> Point
elementwise f (x,y) = (f x, f y)
elementwise2 :: (Double -> Double -> Double) -> Point -> Point -> Point
elementwise2 f (x,y) (v,w) = (f x v, f y w)
accumulate :: (Double -> Double -> Double) -> Point -> Double
accumulate f (x,y) = f x y

-- Vector operations
add :: Point -> Point -> Point
add = elementwise2 (+)
sub :: Point -> Point -> Point
sub = elementwise2 (-)
dot :: Point -> Point -> Double
dot p1 p2 = accumulate (+) $ elementwise2 (*) p1 p2
scalar :: Double -> Point -> Point
scalar s p = elementwise (s *) p
norm :: Point -> Double
norm p = sqrt (p `dot` p)
euclideanDistance :: Point -> Point -> Double
euclideanDistance v1 v2 = norm (v1 `sub` v2)
manhattanDistance v1 v2 = accumulate (+) $ elementwise abs $ sub v1 v2

-- functions for processing strokes
-- concatStrokes :: Strokes -> Stroke
-- concat = concat