-- # Commands
-- Here we learn
--  > Lists
--  > Constructors
--  > Tuples
--  > Tail recursion

-- # Refs
--  > https://youtu.be/AN-P1-IvsKQ

import Data.List -- A lot of list functions reside here

main :: IO ()

-- Using constructors to build lists:
--  There are two of them
--  The first one is the empty List
--  And the collon :, which acts as a Prepend
lst_a :: [Integer]
lst_a=1:2:3:[]

-- Recursevely constructing lists
--  We basically just prepend n recursively:
--
--  Func   n m
--  range  1 4 -> 1 : range 2 4
--  range  2 4 -> 2 : range 3 4
--  range  3 4 -> 3 : range 4 4
--  range  4 4 -> [4]
--  Note that the first condition avoids/handles errors. It returns an empty list if m < n
range :: Int -> Int -> [Int]
range n m
  | m < n   = []
  | m == n  = [m]
  | m > n   = n : range (n+1) m

-- We also have
--
-- head :: [a] -> a
-- which returns the first element of the list

-- And
--
-- tail :: [a] -> [a]
-- Which returns all the remaining elements

-- Finally, we also have:
--
-- length :: [a] -> Int
-- e.g., length [1,2,3,4] = 4
--
-- init :: [a] -> [a]]
-- e.g., init [1,2,3,4] = [1,2,3]
--
-- null :: [a] -> Bool
-- e.g.,  null [] = True
--        null [1,2,3,4] = False
--
-- and :: [Bool] -> Bool
-- e.g.,  and [True, False, True] = False
--
-- or :: [Bool] -> Bool
--        or [True, False, True] = True
-- To make these boolean lists we can use:

-- List Comprehension
--  The basic structure is something like:
--  [ <gen> | <elem> <- <list>, ..., <guard>, ...]
--
-- Example> let's create a functioni which doubles the function and cuts small elements
double_and_cut :: [Int] -> [Int]
double_and_cut y = [ 2*x | x <- y, x > 2 ]

main =
  do
    putStrLn ("List A:" ++ (show lst_a))
    putStrLn ("Range from 1 to 4:" ++ (show lst_out))
    putStrLn ("Head of the previous list:" ++ (show (head lst_out)))
    putStrLn ("Tail of the previous list:" ++ (show (tail lst_out)))
    putStrLn ("Length of the previous list:" ++ (show (length lst_out)))
    putStrLn ("Without the last element of the previous list:" ++ (show (init lst_out)))
    putStrLn ("The doubled list is:" ++ (show [ 2*x | x <- [1, 2, 3, 4] ]))
    putStrLn ("The doubled list without small elements is:" ++ (show (double_and_cut lst_out)))
  where
    lst_out = (range 1 4)
