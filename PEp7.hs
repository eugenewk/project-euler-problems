-- PEp7.hs

primes = [x | x <- [1..], isPrime x == True]

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = not . any divides $ [2..sqrtN]
  where divides d = n `mod` d == 0
        sqrtN = floor . sqrt $ fromIntegral n

fTL :: Integer
fTL = head . reverse $ take 10001 primes 

