--PEp009.hs

import Data.Fixed

pytTrips = [ (a,b,c, a+b+c, a*b*c) | a <- [50..300], b <- [100..400], c <- [500..700], (a < b) == True, (b < c) == True, a^2 + b^2 == c^2, a+b+c == 1000]

isPyth :: Int -> Int -> Int -> Bool
isPyth a b c
  | a^2 + b^2 == c^2 = True
  | otherwise = False

--note this function is accurate at least up to 1000000 but may have issues beyond that due to Floating issues
isPerfSq :: Int -> Bool
isPerfSq n
  | mod' (sqrt (fromIntegral n)) 1 == 0 = True
  | otherwise = False
  
perfSqrs = [ (x, x^2) | x <- [1..1000]]

sumsToM = [ (a,b,c) | a <- [1..1000], b <- [1..1000], c <- [1..1000], a+b+c == 1000, a < b, b < c, a^2 + b^2 == c^2]



