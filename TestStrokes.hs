import Test.QuickCheck
import Test.QuickCheck.Batch

import Strokes
import Sim

instance Arbitrary Point where
  arbitrary = do
    p <- arbitrary
    return $ Point p
  coarbitrary = undefined

forAll_ = flip all

validbox (x1, y1, x2, y2) = x1 < x2 && y1 < y2 

(Point (x, y)) `inside` (x1, y1, x2, y2) = (x >~ x1) && (x <~ x2) && (y >~ y1) && (y <~ y2)

prop_stroke_fits_inside_bounding_box :: Stroke -> Property
prop_stroke_fits_inside_bounding_box stroke = stroke /= [] ==> forAll_ stroke $ \p ->
  p `inside` (boundingbox stroke)

prop_refit_fits_inside :: (Double, Double, Double, Double) -> Stroke -> Property
prop_refit_fits_inside box stroke =
  stroke /= [] ==> validbox box ==> forAll_ (refit box stroke) $ \p -> p `inside` box
    
prop_refit_idempotent :: (Double, Double, Double, Double) -> Stroke -> Property
prop_refit_idempotent box stroke =
  stroke /= [] ==> validbox box ==> refit box stroke ~~ refit box (refit box stroke)

prop_redistribute_works :: Int -> Stroke -> Property
prop_redistribute_works n stroke =
  length stroke > 0 ==> n > 0 ==> length (redistribute n stroke) == n
  
prop_multiredistribute_works :: Int -> Strokes -> Property
prop_multiredistribute_works n strokes =
  length strokes > 0 ==>
  n > length strokes ==>
  all (\s -> length s > 0) strokes ==>
  foldl1 (+) (map length (multiredistribute n strokes)) == n

main = do
 runTests "Stroke" defOpt
  [
  run prop_stroke_fits_inside_bounding_box,
  run prop_refit_fits_inside,
  run prop_refit_idempotent,
  run prop_redistribute_works,
  run prop_multiredistribute_works
  ]