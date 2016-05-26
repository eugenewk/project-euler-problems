-- PEp6.hs
nums = [1..100]
sqrSum = sum (zipWith (*) nums nums)
sumSqr = (sum nums) * (sum nums)
answer = sumSqr - sqrSum