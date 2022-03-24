-- Solution from the previous exercise
isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = isPrime' n (floor $ sqrt $ fromIntegral n) where
  isPrime' n m
    | m == 1 = True
    | m > 1  = (mod n m /= 0) && (isPrime' n (m -1))

-- Brute forces the solution. We look for a number x between n and x for Which
-- both x and n - x are prime.
goldbach :: Int -> (Int, Int)
goldbach n = head [(x, n - x) | x <- [2..n], (isPrime x == True) && (isPrime (n-x) == True)]

-- *Main> goldbach 30
-- (7,23)
-- *Main> goldbach 44
-- (3,41)
-- *Main> goldbach 34
-- (3,31)
