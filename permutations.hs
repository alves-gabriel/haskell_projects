-- Removes the n-th element of the list
removeAt :: Int -> [a] -> [a]
removeAt 0 (x:xs) = xs
removeAt n (x:xs) = x : (removeAt (n-1) xs)

-- Returns all the sublists with one element remove, e.g.
-- removeMap [1, 2, 3] = [[1, 2], [1, 3], [2, 3]]
removeMap :: [a] -> [[a]]
removeMap lst = map (\func -> func lst) f' where
  f' = map removeAt [0..((length lst)-1)]

-- Returns all the permutations. This can be done recursively
perm :: [a] -> [[a]]
perm [] = [[]]
perm lst = concat $ zipWith (\x -> map (x:)) lst (map perm (removeMap lst))

-- The strategy here is as follows: consider the list "lst = [1, 2, 3]". The permutations are:
--
--        [1 + All possible permutations of [2, 3], 2 + All possible permutations of [1, 3], 3 + All possible permutations of [1, 2]]
--
-- So we can see that the sublists [2, 3], [1, 3] and [1, 2] all had a single element removed, and it's obtained by
-- applying removeMap lst.
--
-- If we detone the function for all possible permutations as "perm", we can write:
--
--  perm [1, 2, 3] = [1 + perm [2, 3], 2 + perm [1, 3], 3 + perm [1, 2]]
--
-- This shows how the recursion is implemented. And how can we perform 1 + perm [2, 3]? I.e.,
-- connect the removed element with all  the subpermutations?
--
-- For instance, 1 should be prepended to all the sublists in perm [2, 3] = [[2, 3], [3, 2]],
-- yielding  [[1, 2, 3], [1, 3, 2]] (and we'd get [[2, 1, 3], [2, 3, 1]] for 2 and so on...)
--
-- Here we can use the anonymous function (\x -> map (x:)), since
-- map (1:) [[2, 3], [3, 2]] = [[1, 2, 3],[1, 3, 2]]. Finally, by using ZipWith,
-- we can do this for 1, 2 and 3. I.e., we prepend all the removed elements to
-- the permutations of all the sublists associated with it. For instance,
--

-- zipWith (\x -> map (x:)) [1, 2, 3]
-- [
--  [[2,3],[2,3]]                         -- Maps "1:" here
-- ,[[1,3],[3,1]]                         -- Maps "2:" here
-- ,[[1,2],[2,1]]                         -- Maps "3:" here
-- ]
--
--  yields,
--
-- [
--  [[1,2,3],[1,2,3]]
-- ,[[2,1,3],[2,3,1]]
-- ,[[3,1,2],[3,2,1]]
-- ]
--
-- This algorithm is thus implemented with:
--
-- so zipWith (\x -> map (x:)) lst (map perm (removeMap lst))
--
-- Finally, we have to apply a "concat at the innermost" layer, in order to flatten everything,
-- otherwise we would keep nesting the list because of perm, which has type sig.  [a] -> [[a]].

-- *Main> perm [1, 2, 3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- *Main> perm [1]
-- [[1]]
