module Strokes
  (
  Point(..), Stroke, Strokes, Points,
  add, sub, dot, scalar, norm,
  delta,
  euclideanDistance, manhattanDistance, dist,
  boundingbox, slength, refit, unduplicate, smooth, redistribute, redistribute',
  limit, dropEmpty
  ) where

import Data.List (sort, sortBy)
import Sim

-- data types
newtype Point = Point (Double, Double) deriving (Show, Eq, Ord)
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

translate :: Double -> Double -> Point -> Point
translate x y p = p `add` (Point (x,y))

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

slength :: Stroke -> Double
slength (p:q:ps) = p `dist` q + (slength (q:ps))
slength _ = 0

boundingbox [] = error "An empty stroke has no bounding box"
boundingbox ((Point (x,y)):ps) = foldl step (x,y,x,y) ps where
  step :: (Double, Double, Double, Double) -> Point -> (Double, Double, Double, Double)
  step (minX, minY, maxX, maxY) (Point (x,y)) = (min minX x, min minY y, max maxX x, max maxY y)

-- stroke processors

-- fit into square (x1, y1, x2, y2)
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

-- remove successive duplicate (similar) points
unduplicate :: Stroke -> Stroke
unduplicate [] = []
unduplicate [p] = [p]
unduplicate (p:q:ps) | p ~~ q    = unduplicate (p:ps)
                       | otherwise = p:(unduplicate (q:ps))

-- smooth
smooth :: Stroke -> Stroke
smooth s@(x:y:z:ps) = x:(smooth' s) where
  smooth' (x:y:z:ps) = ((1/3) `scalar` (x `add` y `add` z)):(smooth' (y:z:ps))
  smooth' (x:ps) = ps
smooth ps = ps

-- redistribute to equidistant series by distance
redistribute' :: Double -> Stroke -> Stroke
redistribute' dist _ | dist <= 0 = error "No Sir! No redistribution with non-positive distance!"
redistribute' dist s@(p:q:ps) = p:(redist dist s) where -- first point always part of new stroke
  redist :: Double -> Stroke -> Stroke
  redist left (p:q:ps) | d < left = redist (left - d) (q:ps)
                       | otherwise = ins:(redist dist (ins:q:ps))
                         where
                           dir = q `sub` p
                           d = norm dir
                           ins = p `add` ((left/d) `scalar` dir)
  redist _ ps = ps -- done when only one left
redistribute' _ ps = ps -- empty or single point strokes stay unmodified
                           
-- redistribute to equidistant series by number of points
-- this might go wrong due to numeric inaccuracy (esp. for large n)
-- but together with unduplicate it should work well enough
redistribute :: Int -> Stroke -> Stroke
-- degenerate cases
redistribute _ [] = []
redistribute _ [p] = [p]
-- normal cases
redistribute 0 _ = []
redistribute 1 s = [head s]
redistribute num stroke@(p:q:ps) = redistribute' dist stroke where
  dist = (slength stroke) / ((fromIntegral num) - 1)

dropEmpty xs = filter (/=[]) xs

limit = take
