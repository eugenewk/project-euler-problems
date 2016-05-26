testTree = Branch (Pairs 1 1) Empty Empty

----------------------------------------------------------------------------------------- 

nextTerm :: Int -> Int
nextTerm n
  | even n = div n 2
  | otherwise = 3 * n + 1
  
-----------------------------------------------------------------------------------------

-- call with [2.100] (Branch (Pairs 1 1) Empty Empty)
iterator :: [Int] -> Tree Pairs -> Tree Pairs
iterator [] (Branch a b c) = Branch a b c
iterator (x:xs) (Branch a b c) = iterator xs $ insert (Pairs x $ seqlength x (Branch a b c)) (Branch a b c)

genColSeq :: Int -> [Int]
genColSeq n
  | n == 1 = [n]
  | otherwise = n : (genColSeq . nextTerm $ n)

seqlength :: Int -> Tree Pairs -> Int
seqlength 1 _ = 1
seqlength n (Branch a b c)
  | find n (Branch a b c) == Nothing = 1 + seqlength (nextTerm n) (Branch a b c)
  | otherwise = spsnd a

myLength :: Int -> Int
myLength 1 = 1
myLength n = 1 + (myLength . nextTerm $ n)

test :: Tree Pairs
test = Branch (Pairs 1 1) Empty Empty

getlengths :: Int -> Tree Pairs -> Tree Pairs
getlengths n (Branch a b c)
  | find n (Branch a b c) /= Nothing = Branch a b c
  | otherwise = insert (Pairs n (seqlength n (Branch a b c))) (Branch a b c)
                

------------------------- Pairs def'n and functions -------------------------

data Pairs = Pairs Int Int
                deriving (Show, Eq, Ord)

spfst :: Pairs -> Int
spfst (Pairs a b) = a

spsnd :: Pairs -> Int
spsnd (Pairs a b) = b

------------------------- Tree def'n and handling functions -------------------------

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

-- returns the left branch of a node
lbr :: Tree a -> Tree a
lbr Empty = Empty
lbr (Branch _ Empty _) = Empty
lbr (Branch _ (Branch a b c) _) = Branch a b c

-- returns the right branch of a node
rbr :: Tree a -> Tree a
rbr Empty = Empty
rbr (Branch _ _ Empty) = Empty
rbr (Branch _ _ (Branch a b c)) = Branch a b c

-- sets an empty right branch to a new value
setrbr :: a -> Tree a -> Tree a
setrbr n (Branch a b Empty) = Branch a b (Branch n Empty Empty)

-- sets an empty left branch to a value
setlbr :: a -> Tree a -> Tree a
setlbr n (Branch a Empty b) = Branch a (Branch n Empty Empty) b

-- search for a value
find :: Int -> Tree Pairs -> Maybe Int
find _ Empty = Nothing
find n (Branch a b c)
  | n == spfst a = Just (spsnd a)
  | n <  spfst a = find n b
  | n >  spfst a = find n c

-- inserts a value into a tree in the appropriate location
insert :: Pairs -> Tree Pairs -> Tree Pairs
--insert n Empty = Branch (Pairs n $ seqlength n) Empty Empty
insert n (Branch a b c)
  | spfst n < spfst a && b /= Empty = balance $ (Branch a (insert n b) c)
  | spfst n < spfst a && b == Empty = balance $ setlbr n (Branch a b c)
  | spfst n > spfst a && c /= Empty = balance $ (Branch a b (insert n c))
  | spfst n > spfst a && c == Empty = balance $ setrbr n (Branch a b c)
  | otherwise = Branch a b c
  
-- given a value, delete a node **Will fix when needed. For now, problem does not require deletion.
-- delete :: (Ord a, Eq a) => a -> Tree a -> Tree a
-- delete _ Empty = Empty
-- delete n (Branch a Empty Empty)
  -- | n == a = Empty
  -- | otherwise = Branch a Empty Empty
-- delete n (Branch a b Empty)
  -- | n == a = b 
  -- | otherwise = Branch a (delete n b) Empty
-- delete n (Branch a Empty c)
  -- | n == a = c
  -- | otherwise = Branch a Empty $ delete n c
-- delete n (Branch a b c)
  -- | n < a  = Branch a (delete n b) c
  -- | n > a  = Branch a b $ delete n c 
  -- | n == a = Branch (bval b) (delete (bval b) b) c
  -- where bval (Branch a _ _) = a

------------------------- balancing functions -------------------------

balance :: Tree a -> Tree a
balance Empty = Empty
balance (Branch a b c)
  | balancefactor (Branch a b c) >   1  = rrot (Branch a b c)
  | balancefactor (Branch a b c) < (-1) = lrot (Branch a b c)
  | otherwise = Branch a b c

balancefactor :: Tree a -> Int
balancefactor Empty = 0
balancefactor (Branch a b c) = height b - height c

height :: Tree a -> Int
height Empty = 0
height (Branch _ b c) = 1 + max (height b) (height c)

lrot :: Tree a -> Tree a
lrot Empty = Empty
lrot (Branch a l r) = Branch (bval r) (Branch a l (lbr r)) (rbr r)
                      where bval (Branch a _ _) = a

rrot :: Tree a -> Tree a
rrot Empty = Empty
rrot (Branch a l r) = Branch (bval l) (lbr l) (Branch a (rbr l) r)
                      where bval (Branch a _ _) = a

