module Strokes
  (
  Point(..), Stroke, Strokes, Points,
  add, sub, dot, scalar, norm,
  delta,
  euclideanDistance, manhattanDistance, dist,
  boundingbox, slength, refit, aspectrefit, unduplicate, smooth, redistribute, redistribute', dominant,
  limit, dropEmpty
  ) where

import Data.List (sort, sortBy, foldl')
import Sim

-- data types
newtype Point = Point (Double, Double) deriving (Show, Eq, Ord)
type Points = [Point]
type Rect = (Point, Point)
type Stroke = [Point]
type Strokes = [Stroke]

for = flip map
both f (a, b) = (f a, f b)

xCoord (Point (x,_)) = x
yCoord (Point (_,y)) = y
width ((Point (x1,_)), (Point (x2,_))) = x2 - x1
height ((Point (_,y1)), (Point (_,y2))) = y2 - y1
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
boundingbox ((Point (x,y)):ps) = foldl' step (Point (x,y), Point (x,y)) ps where
  step :: Rect -> Point -> Rect
  step (Point (minX, minY), Point (maxX, maxY)) (Point (x,y)) = (Point (min minX x, min minY y), Point (max maxX x, max maxY y))

-- stroke processors

-- fit into rect (x1, y1, x2, y2)
refit :: Rect -> Stroke -> Stroke
refit (Point (x1, y1), Point (x2, y2)) _ | x1 > x2 || y1 > y2 = error "Dude! Your rect doesn't make sense!"
refit _ [] = [] 
refit (Point (x1, y1), Point (x2, y2)) stroke = for stroke $ \p -> (scale scaleX scaleY (p `sub` reset)) `add` trans
  where
    (Point (bbx1, bby1), Point (bbx2, bby2)) = boundingbox stroke
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

-- fit the first rect maximally and centered into the second rect keeping the aspect ratio
aspectfit :: Rect -> Rect -> Rect
aspectfit source@(a, b) target@(c, d) | a == b = ((1/2) `scalar` (c `add` d), (1/2) `scalar` (c `add` d))
                                      | otherwise = both reposition source where
  reset = fst source
  sourcewidth = width source
  sourceheight = height source
  targetwidth = width target
  targetheight = height target
  sourceratio = sourcewidth/sourceheight
  targetratio = targetwidth/targetheight
  -- bigger ratio <~> wider
  sourcewider = sourceratio > targetratio
  scalefactor | sourcewider = 1/sourcewidth*targetwidth -- scale to fit width of target and center
              | otherwise   = 1/sourceheight*targetheight
  offset      | sourcewider = Point (0, (targetheight - scalefactor*sourceheight)/2)
              | otherwise   = Point ((targetwidth - scalefactor*sourcewidth)/2, 0)
  reposition p = (scale scalefactor scalefactor (p `sub` reset)) `add` offset `add` (fst target)

aspectrefit :: Rect -> Stroke -> Stroke
aspectrefit r s = refit (aspectfit (boundingbox s) r) s

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

dominant :: Double -> Stroke -> Stroke
dominant alpha s@(x:y:z:ps) = x:(dominant' alpha s) where
  dominant' alpha (x:y:z:ps) | angle x y z < alpha = dominant' alpha (x:z:ps)
                             | otherwise = y:(dominant' alpha (y:z:ps))
  dominant' _ (x:ps) = ps
  -- angle x y z = acos (min 1 (max (-1) (1/((norm v)*(norm w)) `scalar` v `dot` w))) where
  angle x y z = acos $ fixdomain $ (v `dot` w)/((norm v)*(norm w)) where
    fixdomain = (min 1).(max (-1))
    v = y `sub` x
    w = z `sub` y
dominant _ ps = ps


dropEmpty xs = filter (/=[]) xs

limit = take
