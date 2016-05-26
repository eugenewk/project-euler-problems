-- PEp5.hs

digs = [11..20]
initVal = 21

fTL :: [Int] -> Int -> Int
fTL xs n
  | checkDiv xs n == True = n
  | otherwise = fTL xs (n+20) 

checkDiv :: [Int] -> Int -> Bool
checkDiv (x:[]) n
  | rem n x == 0 = True
  | otherwise = False
checkDiv (x:xs) n
  | rem n x == 0 = checkDiv xs n
  | otherwise = False
  
