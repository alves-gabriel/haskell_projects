-- # Commands & Topics
-- Here we learn
--  > Function composition (the . operators)
--
-- # Hints:
--
-- # Refs
--  > https://www.youtube.com/watch?v=dR_aUQXw5fs
--  > https://typeclasses.com/featured/dollar
--  > https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign

import Data.List

main :: IO()

-- # Function composition
--  The idea behind this is the following equivalence relation formo the dot/composition operator:
--
--  (.) :: (b->c) -> (a ->b ) -> a -> c
--
--  so, (f . g) is equivalent to (\x -> f (g x))
--
--  So we first apply g to x and then f. For instance, suppose we have a "reverse" and a "sort" function.
--  The following three definitions are equivalent:
--
--  descSort = reverse . sort
--  descSort = (\x -> reverse (sort x))
--  descSort x = reverse (sort x)
--
--  Note how we did not need to explicity write the argument. Now, let's try to write a 2D generalization of map.
--  It's not so easy to see why the definition above works, so let's expand this in a few ways.
--
--  - We begin by rewriting the map as an anonymous function
--  map2D = (\f1 xs -> map f1 xs) . (\f2 ys -> map f2 ys)
--
--  - Now we use the definition for the . operator
--  map2D = (\x ->  (\f1 xs -> map f1 xs) ((\f2 ys -> map f2 ys) x))
--
--  - Now we take the free variable x and explicity put it as an argument of map2D
--   map2D x = (\f1 xs -> map f1 xs) ((\f2 ys -> map f2 ys) x)
--
--  - Now, since we have an anonymous definition on the second term f2, we can put x as its first argument. So
--  we replace f2 by x.
--  map2D x = (\f1 xs -> map f1 xs) (\ys -> map x ys)
--
--  - And we replace f1 with the second anonymous function on the right,so
--  map2D x = (\xs -> map (\ys -> map x ys) xs)
--
--  - And xs is explicity put as an argument on the LHS as well (and we rename x to f):
--  map2D f xs = map (\ys -> map f ys) xs
--                   \---------------/
--                           |
--                           v
--              Note that this term is also a function,
--              since map f x always has a function as
--              the first argument. The reasoning holds for
--              other steps as well.
--
--  Naturally, xs must be a list of lists!
map2D :: (a -> b) -> [[a]] -> [[b]]
map2D = map . map

-- We are going to use use map together with a function which subtracts 1 from each element
--
--  So, effectively, map2D sub1 lst is a function where subtracts 1 from every element
--  in lst, and here lst is effectively a matrix. The naive approach map sub1 lst wouldn't
--  work because Haskell without be able to directly subtract 1 from the rows (i.e. lists) within lst.
--  You could for instance do something like map head lst to get the first element in each row of the matrix
--  i.e., the heads, but not something like map sub1 lst, because the operation (\x -> x - 1) is not
--  valid for lists, only floats.
sub1 = (\x -> x - 1)

-- # Dolar sign
--  This operation is useful for tedious syntax. This is what we used for higher order functions.
--  But we can do it in a clean and nice way.
--  ($) :: (a -> b) -> a -> b
--
--  For example, both these definitions are the same:
--  f xs = map (\x ->\x + 1) (filter (\x -> x>1) xs)
--  f xs = map (\x ->\x + 1) $ filter (\x -> x>1) xs
--
--  The syntax now is much cleaner, since we could remove the parenthesis!
--  The dollar sign is, in other words, an infix notation for function application!
--  f $ x = f x
--
--  Inside main we do another example, namely, sort $ [3, 2, 1] ++ [6, 5, 4].
--  The $ let us omit the parenthesis after sort.

main =
  do
    putStrLn ("The application of 2D map to [[1,2,3],[4,5,6]] is: " ++ (show (map2D sub1 [[1,2,3],[4,5,6]])))
    putStrLn ("The incorrect sorting of [3, 2, 1] ++ [6, 5, 4] is: " ++ (show (sort [3,2,1] ++ [6,5,4])))
    putStrLn ("The correct sorting of [3, 2, 1] ++ [6, 5, 4] is: " ++ (show (sort $ [3,2,1] ++ [6,5,4])))
