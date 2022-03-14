my_gcd :: Int -> Int -> Int
my_gcd a 0 = a
my_gcd a b = my_gcd b (a `mod` b)

{-
*Main> my_gcd 4 32
4
*Main> my_gcd 5 44
1
*Main> my_gcd 36 162
18
}
