-- # Refs
--  > https://www.youtube.com/watch?v=Cxkqrg8FCt8

main :: IO()

-- # Exercise 1
--  Create a function which returns true if a given element is in a list
--  Here "Eq a" is what we call a type clause
--
--  The strategy here is also recursive. The base case is the empty set.
--  No element belongs to [], so we can write this using pattern matching
--  as "is_elem _ [] = False". And the rest is just recursion, so, for instance:
--
--    is_elem 3 [1, 2, 3]
--    (3 == 1) || is_elem 3 [2, 3]
--    (3 == 1) || (3 == 2) || is_elem 3 [3]
--    (3 == 1) || (3 == 2) ||(3 == 3) || is_elem 3 []
--    False    || False    || True    || False
--    True
is_elem :: (Eq a) => a -> [a] -> Bool
is_elem _ [] = False
is_elem el (x:x_rest) = (el == x) || is_elem el x_rest

-- An alternative implementation I saw on youtube, by Anders Gralund
is_elem_alt el lst = or [ el == ee | ee <- lst ]

-- # Exercise 2
--  Create a function which removes all the duplications in a list
--  Note that my implementation here is "appears first deletes first"
--  So only the elements that appear >later< on in the list that remain
--
-- Let's think:
--  del_duplicates [1,1,2,3,2]
--  del_duplicates [1,2,3,2]
--  1:del_duplicates [2,3,2]
--  1:del_duplicates [3,2]
--  1:3:del_duplicates [2]
--  1:3:2:del_duplicates []
--  1:3:2:[] = [1,3,2]
del_duplicates :: (Eq a) => [a] -> [a]
del_duplicates [] = []
del_duplicates (x:xs)  =
  if is_elem x xs then
    del_duplicates xs
  else
    x:del_duplicates xs

-- Here I can define a function which reverses the list:
--  I introduced the "append" operator ++
list_rev :: (Eq a) => [a] -> [a]
list_rev [] = []
list_rev (x:xs) = (list_rev xs)++[x]

-- Now we can define a delete_duplicates which removes elements which appear
-- later on, instead of elements which appear later. Which just reverse the list
-- twice. And we are done!
del_duplicates_good :: (Eq a) => [a] -> [a]
del_duplicates_good lst = list_rev (del_duplicates (list_rev lst))

-- # Exercise 3
--  Create a function which checks if a given list is in ascending order
--
ascQ :: [Integer] -> Bool
ascQ [_] = True
ascQ (x:x_rest) = (x < head x_rest) && ascQ x_rest

-- Alternative solution from Hagenlocher's channel:
ascQ_alt :: [Integer] -> Bool
ascQ_alt [] = True
ascQ_alt [x] = True
ascQ_alt (x:y:x_rest) = (x<=y) && ascQ_alt (y:x_rest)

main =
  do
    putStrLn ("Is 3 in [1, 2, 3]? " ++ (show (is_elem 3 [1,2,3])))
    putStrLn ("Is 4 in [1, 2, 3]? " ++ (show (is_elem 4 [1,2,3])))
    putStrLn ("Is 2 in [1, 2, 3]? " ++ (show (is_elem_alt 2 [1,2,3])))
    putStrLn ("Is 7 in [1, 2, 3]? " ++ (show (is_elem_alt 7 [1,2,3])))
    putStrLn ("After deleting duplicates of [1,1,2,3,2] it becomes " ++ (show (del_duplicates [1, 1, 2, 3, 2])))
    putStrLn ("The reverse of [1, 2, 3] is " ++ (show (list_rev [1,2,3])))
    putStrLn ("After deleting duplicates of [1,1,2,3,2] (in proper order), it becomes " ++ (show (del_duplicates_good [1, 1, 2, 3, 2])))
    putStrLn ("Is [1, 2, 3] ascending? " ++ (show (ascQ [1,2,3])))
    putStrLn ("Is [1, 3, 2] ascending? " ++ (show (ascQ [1,3,2])))
    putStrLn ("Is [3, 1, 2] ascending? " ++ (show (ascQ [3,1,2])))
    putStrLn ("Is [1, 5, 7] ascending? " ++ (show (ascQ_alt [1,5,7])))
