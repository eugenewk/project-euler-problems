--PEp017.hs

answer = sum [ entry x | x <- [1..1000]]

entry :: Int -> Int
entry = length . writeItOut . digs

f :: Int -> String
f = writeItOut . digs

--digs - to break number into list of its digits

digs :: Int -> [Int]
digs 0 = []
digs x = digs (div x 10) ++ [mod x 10] 

--function to convert the numbers into words

writeItOut :: [Int] -> String
writeItOut [] = ""
writeItOut (a:[])     = ones a
writeItOut (a:b:[])   = tens a b
writeItOut (a:b:c:[]) = hundos a b c
writeItOut (a:b:c:d)  = "onethousand"

ones :: Int -> String
ones 0 = ""
ones 1 = "one"
ones 2 = "two"
ones 3 = "three"
ones 4 = "four"
ones 5 = "five"
ones 6 = "six"
ones 7 = "seven"
ones 8 = "eight"
ones 9 = "nine"

tens :: Int -> Int -> String
tens 0 b = ones b 
tens 1 b
  | b == 0 = "ten"
  | b == 1 = "eleven"
  | b == 2 = "twelve"
  | b == 3 = "thirteen"
  | b == 4 = "fourteen"
  | b == 5 = "fifteen"
  | b == 6 = "sixteen"
  | b == 7 = "seventeen"
  | b == 8 = "eighteen"
  | b == 9 = "nineteen"
tens a b
  | a == 2 = "twenty" ++ ones b
  | a == 3 = "thirty" ++ ones b
  | a == 4 = "forty" ++ ones b
  | a == 5 = "fifty" ++ ones b
  | a == 6 = "sixty" ++ ones b
  | a == 7 = "seventy" ++ ones b
  | a == 8 = "eighty" ++ ones b
  | a == 9 = "ninety" ++ ones b
  
hundos :: Int -> Int -> Int -> String
hundos a 0 0 = ones a ++ "hundred"
hundos a b c = ones a ++ "hundredand" ++ tens b c

mills :: Int -> Int -> Int -> [Int] -> String
mills a b c (d:_) = ones a ++ "t"



--functions to check ones, tens, hundreds, can do a special case for 1000

--function to calculate the length of the letter array

--need logic for adding "and"
-- 1	one
-- 2	two
-- 3	three
-- 4	four
-- 5	five
-- 6	six
-- 7	seven
-- 8	eight
-- 9	nine
-- 10	ten   ------------start oddities
-- 11	eleven
-- 12	twelve
-- 13	thirteen
-- 14	fourteen
-- 15	fifteen
-- 16	sixteen
-- 17	seventeen
-- 18	eighteen
-- 19	nineteen     ------------end oddities
-- 20  twenty
-- 30	thirty
-- 40	forty
-- 50	fifty
-- 60	sixty
-- 70	seventy
-- 80	eighty
-- 90	ninety
-- x00	x hundred
-- 1000    one thousand
