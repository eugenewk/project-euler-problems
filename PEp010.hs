--PEp010.hs
--basic sieve of eratosthenes

primeList = [ x | x <- [2..2000000] , isPrime x]

isPrime :: Int -> Bool
isPrime n 
  | n <= 1 = False
  | otherwise = not . any divides $ [2..sqrtN]
  where divides d = mod n d == 0
        sqrtN = floor $ sqrt $ fromIntegral n 

-----------------------------------------------------------------------------------

findprimes :: [Int] -> [Int]
findprimes [] = []
findprimes (x:xs) = x : findprimes (filter keep $ xs)
                        where keep d = rem d x /= 0
