-- A not very efficient implementation due to the use of append

-- Binary operation to decide wheter the argument to the right should be packed
-- into a sublist of the list to the right. E.g.,
-- pack_join [[1], [2,2]] 3 = [[1], [2, 2], [3]], while
-- pack_join [[1], [2,2]] 2 =  [[1], [2, 2, 2]]
pack_join :: (Eq a) => [[a]] -> a -> [[a]]
pack_join x_lst y
  | y == head (last x_lst) = init x_lst ++ [last x_lst ++ [y]]
  | otherwise              = x_lst ++ [[y]]

--  A foldl implementation using the function above. Example:
--  pack [1, 2, 2, 3]
--  [[1]] ⊕ 2 ⊕ 2 ⊕ 3         , since [[head lst]] = 1
--  [[1], [2]] ⊕ 2 ⊕ 3        , since init x = [] and last x = [1], in the previous step
--  [[1],[2, 2]] ⊕ 3          , since init x = [[1]] and last x = [2], in the previous step
--  [[1], [2, 2], [3]]
pack :: (Eq a) => [a] -> [[a]]
pack []  = []
pack lst = foldl pack_join [[head lst]] (tail lst)

-- *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
-- *Main> pack [1, 2, 3]
-- [[1],[2],[3]]
