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

prop_refit_fits_inside :: (Double, Double, Double, Double) -> Stroke -> Property
prop_refit_fits_inside box stroke =
  stroke /= [] ==> validbox box ==> for (refit box stroke) $ \p -> p `inside` box
    
prop_refit_idempotent :: (Double, Double, Double, Double) -> Stroke -> Property
prop_refit_idempotent box stroke =
  stroke /= [] ==> validbox box ==> refit box stroke ~~ refit box (refit box stroke)

prop_redistribute_works_on_clean_stroke :: Int -> Stroke -> Property
prop_redistribute_works_on_clean_stroke n stroke =
  length stroke > 0 ==> n > 0 ==> length (redistribute n $ cleanStroke stroke) == n
    
prop_multiredistribute_works_on_clean_strokes :: Int -> Strokes -> Property
prop_multiredistribute_works_on_clean_strokes n strokes =
  length strokes > 0 ==>
  n > length strokes ==>
  all (\s -> length s > 0) strokes ==>
  foldl1 (+) (map length (multiredistribute n $ cleanStrokes strokes)) == n

main = do
  quickCheck prop_stroke_fits_inside_bounding_box
  quickCheck prop_refit_fits_inside
  quickCheck prop_refit_idempotent
  quickCheck prop_redistribute_works_on_clean_stroke
  quickCheck prop_multiredistribute_works_on_clean_strokes