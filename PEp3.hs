-- projecteuler problem 3

--need to import Data.Numbers.Primes

import Data.List

--need to show

--need isPrime function

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = not . any divides $ [2..sqrtN]
  where divides d = n `mod` d == 0
        sqrtN = floor . sqrt $ fromIntegral n

--build the list from sqrt n/2 to 1 encompassing only odd numbers

buildList :: Integer -> [Integer]
buildList n = [ x | x <- [1..(floor . sqrt $ fromIntegral n)], mod n x == 0, isPrime x == True]

--iterate through the list and return the first prime factor
	
findLPF :: Integer -> Integer
findLPF n = maximum (buildList n)