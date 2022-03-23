-- Function to calculate the GCD between two numbers. See problem 32
my_gcd :: Int -> Int -> Int
my_gcd a 0 = a
my_gcd a b = my_gcd b (a `mod` b)

-- Here we use 'filter' to extract all the elements x in 1, 2, 3, ..., m
-- for which GCD(m, x) == 1, i.e. the coprime numbers between 1 and m
coprimes :: Int -> [Int]
coprimes m = filter (\x -> my_gcd x m == 1) [1..m]

-- The totient phi funciton is just the number of coprimes number between 1 and m
totient :: Int -> Int
totient m = length (coprimes m)

-- *Main> coprimes 9
-- [1,2,4,5,7,8]
-- *Main> coprimes 10
-- [1,3,7,9]
-- *Main> totient 10
-- 4
