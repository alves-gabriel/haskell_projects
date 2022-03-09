-- Replicate the elements of a list a given number of times.
-- Example in Haskell:
--
-- λ> repli "abc" 3
-- "aaabbbccc"

-- Solution 1
nreplicate :: [a] -> Int -> [a]
nreplicate [] _ = []
nreplicate (x:xs) n = aux_repli (x:xs) n n where
  aux_repli [] _ _ = []
  aux_repli (x:xs) n m
    | m == 0    = nreplicate xs n
    | otherwise = x : aux_repli (x:xs) n (m-1)

-- *Main> nreplicate "abc" 0
-- ""
-- *Main> nreplicate "abc" 1
-- "abc"
-- *Main> nreplicate "abc" 3
-- "aaabbbccc"
