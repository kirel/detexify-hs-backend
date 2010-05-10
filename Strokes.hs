module Strokes
  (
  Point(..), Stroke, Strokes, Points,
  add, sub, dot, scalar, norm,
  euclideanDistance, manhattanDistance, dist,
  boundingbox, refit
  ) where

import Sim

-- data types
newtype Point = Point (Double, Double) deriving (Show, Eq)
type Stroke = [Point]
type Strokes = [Stroke]
type Points = [Point]  

for = flip map

-- x (Point (x,_)) = x
-- y (Point (_,y)) = y
-- elementwise = zipWith for Points
elementwise :: (Double -> Double) -> Point -> Point
elementwise f (Point (x,y)) = Point (f x, f y)
elementwise2 :: (Double -> Double -> Double) -> Point -> Point -> Point
elementwise2 f (Point (x,y)) (Point (v,w)) = Point (f x v, f y w)
accumulate :: (Double -> Double -> Double) -> Point -> Double
accumulate f (Point (x,y)) = f x y

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
dist = euclideanDistance

-- Vector maps
vendomorphism :: ((Double, Double), (Double, Double)) -> Point -> Point
vendomorphism ((a11, a12), (a21, a22)) (Point (x, y)) = Point (a11*x + a12*y, a21*x + a22*y)

scale :: Double -> Double -> Point -> Point
scale x y = vendomorphism ((x, 0), (0, y))

-- Stroke similarity
delta = 1e-10
instance Sim Double where
  a ~~ b = abs (a - b) < delta
instance Simord Double
instance Sim Point where
  p ~~ q = (p `dist` q) < delta
instance (Sim a) => Sim [a] where
  [] ~~ [] = True
  (p:ps) ~~ (q:qs) = (p ~~ q) && (ps ~~ qs)
  _ ~~ _ = False

-- feature extractors

slength (p:q:ps) = p `dist` q + (slength (q:ps))
slength _ = 0

boundingbox [] = error "An empty stroke has no bounding box"
boundingbox ((Point (x,y)):ps) = foldl step (x,y,x,y) ps where
  step :: (Double, Double, Double, Double) -> Point -> (Double, Double, Double, Double)
  step (minX, minY, maxX, maxY) (Point (x,y)) = (min minX x, min minY y, max maxX x, max maxY y)

-- functions for processing strokes
-- concatStrokes :: Strokes -> Stroke
-- concat = concat

-- stroke processors

-- fit into square (n, e, s, w)
refit :: (Double, Double, Double, Double) -> Stroke -> Stroke
refit (x1, y1, x2, y2) _ | x1 > x2 || y1 > y2 = error "Dude! Your square doesn't make sense!"
refit _ [] = [] 
refit (x1, y1, x2, y2) stroke = for stroke $ \p -> (scale scaleX scaleY (p `sub` reset)) `add` trans
  where
    (bbx1, bby1, bbx2, bby2) = boundingbox stroke
    reset = Point (bbx1, bby1)
    bbWidth = bbx2 - bbx1
    bbHeight = bby2 - bby1
    targetWidth = x2 - x1
    targetHeight = y2 - y1
    scaleX = case bbWidth of
      0 -> 1
      width -> 1/width * targetWidth
    scaleY = case bbHeight of
      0 -> 1
      height -> 1/height * targetHeight
    transX = case bbWidth of
      0 -> x1 + 1/2 * targetWidth
      width -> x1
    transY = case bbHeight of
      0 -> y1 + 1/2 * targetHeight
      height -> y1
    trans = Point (transX, transY)

redistribute :: Double -> Stroke -> Stroke
redistribute _ [] = []
redistribute dist s@(p:ps) = p:(redist dist s) where -- first point always part of new stroke
  redist :: Double -> Stroke -> Stroke
  redist _ [] = []
  redist _ [q] = [] -- last point is always discarded
  redist left (p:q:ps) | d < left = redist (left - d) (q:ps)
                       | otherwise = ins:(redist dist (ins:q:ps))
                         where
                           dir = q `sub` p
                           d = norm dir
                           ins = p `add` ((left/d) `scalar` dir)