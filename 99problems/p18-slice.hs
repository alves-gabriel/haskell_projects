-- See p16, similar construction
sliceN :: [a] -> Int -> [a]
sliceN (x:xs) n = sliceN' (x:xs) n $ n-1 where
  sliceN' [] _ _ = []
  sliceN' _ 0 _ = []
  sliceN' (x:xs) nmax n
    | n >= 0 =
      if n == 0 then
        x : (sliceN' (xs) nmax $ nmax-1)  -- Prepends the element x (index multiple of n).
      else
        sliceN' (xs) nmax $ n-1           -- Ignores the urrent element and moves on.
    | otherwise =                         -- If the n < 0, goes through the list in reverse order
      sliceN (reverse (x:xs)) $ (-n-1)

-- *Main> sliceN [1..10] 2
-- [2,4,6,8,10]
-- *Main> sliceN [1..10] 7
-- [7]
-- *Main> sliceN [1..10] $ -1
-- [10,9,8,7,6,5,4,3,2,1]
-- *Main> sliceN [1..10] 0
-- []
