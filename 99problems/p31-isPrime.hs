isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = isPrime' n (floor $ sqrt $ fromIntegral n) where -- fromIntegral is necessary since sqrt has a Floating point as an input
  isPrime' n m
    | m == 1 = True                                          -- Recursive solution
    | m > 1  = (mod n m /= 0)&&(isPrime' n (m -1))

-- *Main> isPrime 2
-- True
-- *Main> isPrime 3
-- True
-- *Main> isPrime 1
-- False
-- *Main> isPrime 5
-- True
-- *Main> isPrime 9
-- False
-- *Main> isPrime 11
-- True
-- *Main> isPrime 33
-- False
-- *Main> isPrime 37
-- True
