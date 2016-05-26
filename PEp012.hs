--PEp012.hs

triNums :: Int -> Int
triNums 0 = 0
triNums n = n + triNums (n-1)

listGen :: [Int]
listGen = [ x | x <- map (triNums) [1..]]

countDivs :: Int -> Int
countDivs 0 = 0
countDivs 1 = 1
countDivs n
  | n < 0 = -1
  | otherwise = divSum n [1..n]

divSum :: Int -> [Int] -> Int
divSum n [] = 0
divSum n xs = length (filter (divides) xs)
              where divides d = mod n d == 0

