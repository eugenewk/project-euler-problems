-- projectEuler p4

--make a list of all possible combinations of products of digit numbers
threeDigProds = [ (x * y, x, y) | x <- [999,998..800] , y <- [999,998..800] , wrapper (x*y) == True]

--wrapper :: Int -> Bool
wrapper n 
  | digs n == reverse (digs n) = True
  | otherwise = False

--splits it into a list of digits
--digs :: Int -> [Int]
digs 0 = []
digs x = digs (div x 10) ++ [mod x 10]

--answer
answer = maximum threeDigProds

