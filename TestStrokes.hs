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
  stroke /= [] ==> validbox box ==> forAll_ (refit box stroke) $ \p ->
    p `inside` box
    
prop_refit_idempotent :: (Double, Double, Double, Double) -> Stroke -> Property
prop_refit_idempotent box stroke =
  stroke /= [] ==> validbox box ==> refit box stroke ~~ refit box (refit box stroke)


main = do
 runTests "Stroke" defOpt
  [
  run prop_stroke_fits_inside_bounding_box,
  run prop_refit_fits_inside,
  run prop_refit_idempotent
  ]