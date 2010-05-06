module LB
  (
  hullSeries, dtwlb, naivedtw,
  ConvexHull
  ) where

import Data.Maybe
import Data.List (sortBy, transpose, nub, foldl')
import Strokes

newtype ConvexHull = ConvexHull [Point] deriving (Show)

-- lower bounding

convex :: Point -> Point -> Point -> Bool
convex (Point (vx, vy)) (Point (wx, wy)) (Point (ux, uy)) = ( wx - vx ) * ( uy - vy ) - ( wy - vy ) * ( ux - vx ) > 0
concave :: Point -> Point -> Point -> Bool
concave v w u = not (convex v w u)

angle (Point (0, 0)) = 1/0 -- Infinity
angle (Point (x, y)) = x/y

-- ccw trajectory of the convex hull
grahamConvexHull :: Points -> ConvexHull
-- grahamConvexHull [_] = [_]
grahamConvexHull points | length points < 4 = ConvexHull $ nub points
grahamConvexHull points = ConvexHull $ reverse $ nub $ foldl step [] sortedPoints where
                        minP = foldl1 st points where -- checked!
                          st (Point (minPx, minPy)) (Point (px, py)) | py < minPy || (py == minPy && px < minPx) = Point (px, py)
                                                     | otherwise = Point (minPx, minPy)
                        comp v w = compare (negate $ angle v, norm v) (negate $ angle w, norm w)
                        sortedPoints = map (`add` minP) $ sortBy comp $ map (`sub` minP) points
                        step :: Points -> Point -> Points
                        step hull point | length hull <= 2 = point:hull -- p0 and p1 already convex!
                                        | convex (head (tail hull)) (head hull) point = point:hull
                                        | otherwise = point:(tail hull) -- drop point in the middle

toMaybeList :: [a] -> [Maybe a]
toMaybeList [] = []
toMaybeList (x:xs) = (Just x):(toMaybeList xs)

-- the MaybeLists must habe equal lengths for this
-- to work as intended
zipMaybeList :: [[Maybe a]] -> [[a]]
zipMaybeList = (map catMaybes) . transpose

shiftLeft :: [Maybe a] -> [Maybe a]
shiftLeft list = (tail list) ++ [Nothing]

shiftRight :: [Maybe a] -> [Maybe a]
shiftRight list = [Nothing] ++ (init list)

shiftedSeries :: [a] -> Int -> [[a]]
shiftedSeries list width = zipMaybeList $ [maybelist] ++ leftshifted ++ rightshifted where
                            maybelist = toMaybeList list
                            leftshifted = take width $ iterate shiftLeft (shiftLeft maybelist)
                            rightshifted = take width $ iterate shiftRight (shiftRight maybelist)

hullSeries :: Int -> Stroke -> [ConvexHull]
hullSeries width list = map grahamConvexHull $ shiftedSeries list width

-- hull distance -- THIS IS THE PART WORTH OPTIMIZING
pairs :: [a] -> [(a,a)]
pairs list = zip list (tail list)

minimumDistancePoint :: Point -> (Point, Point) -> Point
minimumDistancePoint point (l1, l2) = nearest where
                      param = ((point `sub` l1) `dot` (l2 `sub` l1)) / ((euclideanDistance l1 l2)**2)
                      nearest = case param of
                        u | u <= 0 -> l1
                          | u >= 1 -> l2
                          | otherwise -> l1 `add` (u `scalar` (l2 `sub` l1))
                          
pointLineSegmentDistance :: Point -> (Point, Point) -> Double
pointLineSegmentDistance point (l1, l2) = euclideanDistance point nearest where
                          nearest = minimumDistancePoint point (l1, l2)

-- relevantLines may be empty if point lies inside the hull
minimumOrZero [] = 0.0
minimumOrZero l = minimum l

removeRepeating l = foldl' step [] l where
  step [] n = [n]
  step l@(e:es) n | e == n = l
                  | otherwise = n:l

-- FIXME This is the function that needs optimization!
pointHullDistance :: Point -> ConvexHull -> Double
pointHullDistance point (ConvexHull []) = error "Empty Hull!?"
pointHullDistance point (ConvexHull [other]) = euclideanDistance point other
-- pointHullDistance point (ConvexHull hull) = minimumOrZero $ map (pointLineSegmentDistance point) relevantLines where
pointHullDistance point (ConvexHull hull) =
    minimumOrZero $
    map ((euclideanDistance point) . (minimumDistancePoint point)) relevantLines where
      relevantLines = filter predicate $ take (length hull) $ pairs $ cycle hull where
        predicate (l1, l2) = concave l1 l2 point

-- Stroke -> Hulls -> Double
dtwlb :: Stroke -> [ConvexHull] -> Double
dtwlb stroke hulls = foldl (+) 0 $ zipWith pointHullDistance stroke hulls

for = flip map

naivedtw :: Eq a => ( a -> a -> Double ) -> Int -> [a] -> [a] -> Double
naivedtw measure w s o = foldl1 (+) $ for (zip s o') $ \(p, ps) ->
  minimum $ for ps $ \p' -> measure p p' 
  where
    o' = shiftedSeries o w
