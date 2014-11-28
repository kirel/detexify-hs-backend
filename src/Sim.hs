module Sim where

class Sim a where
  (~~), (/~) :: a -> a -> Bool
  -- Minimal complete defintion:
  --      (==) or (/=)
  x /~ y           =  not (x ~~ y)
  x ~~ y           =  not (x /~ y)
  
class (Sim a, Ord a) => Simord a where
  (<~), (>~) :: a -> a -> Bool
  -- a <~ b && a >~ b == a ~~ b
  a <~ b = a < b || a ~~ b
  a >~ b = a > b || a ~~ b  