-- Here dropN' is an auxiliary function which keeps track of the index and tvalue of N
dropN :: [a] -> Int -> [a]
dropN (x:xs) n = dropN' (x:xs) n $ n-1 where
  dropN' [] _ _ = []
  dropN' (x:xs) nmax n
    | n <= 0  = dropN' (xs) nmax $ nmax-1
    | n > 0   = x : (dropN' (xs) nmax $ n-1)

{-
*Main> dropN [1..6] 2
[1,3,5]
*Main> dropN [1..10] 5
[1,2,3,4,6,7,8,9]
*Main> dropN [1..10] 1
[]
*Main> dropN [1..10] 0
[]
}
