import Test.QuickCheck

import Strokes
import Sim

instance Arbitrary Point where
  arbitrary = do
    p <- arbitrary
    return $ Point p

for = flip all

validbox (x1, y1, x2, y2) = x1 < x2 && y1 < y2 

(Point (x, y)) `inside` (x1, y1, x2, y2) = (x >~ x1) && (x <~ x2) && (y >~ y1) && (y <~ y2)

prop_stroke_fits_inside_bounding_box :: Stroke -> Property
prop_stroke_fits_inside_bounding_box stroke = stroke /= [] ==> for stroke $ \p ->
  p `inside` (boundingbox stroke)

prop_refit_into_boundingbox_is_identity :: Stroke -> Property
prop_refit_into_boundingbox_is_identity stroke =
  stroke /= [] ==>
  refit (boundingbox stroke) stroke ~~ stroke

prop_refit_fits_inside :: (Double, Double, Double, Double) -> Stroke -> Property
prop_refit_fits_inside box stroke =
  stroke /= [] ==> validbox box ==>
  for (refit box stroke) $ \p -> p `inside` box
    
prop_refit_idempotent :: (Double, Double, Double, Double) -> Stroke -> Property
prop_refit_idempotent box stroke =
  stroke /= [] ==> validbox box ==> refit box stroke ~~ refit box (refit box stroke)

prop_redistdribute'_preserves_first_point :: Double -> Stroke -> Property
prop_redistdribute'_preserves_first_point d stroke =
  d > 3 * delta ==>
  slength stroke > 0 ==>
  head (redistribute' d stroke) ~~ head stroke

prop_redistdribute'_preserves_last_point :: Double -> Stroke -> Property
prop_redistdribute'_preserves_last_point d stroke =
  d > 3 * delta ==>
  slength stroke > 0 ==>
  last (redistribute' d stroke) ~~ last stroke

main = do
  quickCheck prop_stroke_fits_inside_bounding_box
  quickCheck prop_refit_fits_inside
  quickCheck prop_refit_into_boundingbox_is_identity
  quickCheck prop_refit_idempotent
  quickCheck prop_redistdribute'_preserves_first_point 
  quickCheck prop_redistdribute'_preserves_last_point 