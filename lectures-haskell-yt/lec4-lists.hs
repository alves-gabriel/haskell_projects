-- # Commands
-- Here we learn
--  > Lists
--  > Constructors
--  > Tuples
--  > Tail recursion
--
-- # Hint:
-- import Data.List -- A lot of list functions reside here
--
-- # Refs
--  > https://youtu.be/AN-P1-IvsKQ
--  > https://www.cantab.net/users/antoni.diller/haskell/units/unit02.html
--  > https://en.wikibooks.org/wiki/Haskell/Lists_and_tuples

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
-- <- is sometimes read as bind, is drawn from, gets etc. See: https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators
--
-- This operations ""<elem> <- <...>" is sometimes called generator (https://wiki.haskell.org/List_comprehension)
--
-- Example: let's create a function which doubles the function and cuts small elements
double_and_cut :: [Int] -> [Int]
double_and_cut y = [ 2*x | x <- y, x > 2 ]

-- Tuples
--
-- Note that changing the order of generators also changes the outputs, e.g.,
--
-- Given a = [1, 2, 3] and y = [4, 5], one has
--
-- tuple a b = [ (x,y) | x <- a, y <-b], which yields
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
--
-- while, tuple_rev a b = [ (x,y) | y <-b, x<-a] yields
-- [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]
--
-- The rightmost generator seems to be looped first! I.e. in tuple a b we first fix
-- x = 1 (the first value) and then loop through y = [4, 5]

tuple a b = [ (x,y) | x <- a, y <- b ]

-- List Patterns
--  Here we define a list which sums the elements in a list.
--  In this case, sum(x + rest of list) = x + sum of rest of list
--  So this is also a recursive-like implementation
--
-- sum [1, 2, 3]
-- 1 + sum [2, 3]
-- 3 + sum [3]
-- 6 + sum [] = 6 + 0 = 6
--
-- Note that this implementation is equivalent to:
--
-- sum lst
--   | lst == []  = 0
--   | otherwise = head lst + sum (tail lst)
--
-- Which yields the same result as our pattern matching approach
total :: [Int] -> Int
total []      = 0
total (x:xs)  = x + total xs

-- A function which returns the even elements
evens :: [Int] -> [Int]
evens []          = []            -- Base case
evens (x:xs)
  | mod x 2 == 0  = x : evens xs  -- If even, prepents it
  | otherwise     = evens xs      -- Otherwise, ignores it and moves on to xs

-- Tuples
--  They are way of having two elements in a pair (or more).
--  e.g., (5,.44) :: (Int,Float)
--  They can be conveniently used in patter matching
--  Another important fact is that, differently from the lists, tuples are immutable
--  we can't change their length, for instance. Also, the elements need not to be of the same type
--
-- The two functions below are already built-in though, as fst and snd
fst_tuple :: (a,b) -> a
fst_tuple (x,_) = x

snd_tuple :: (a,b) -> b
snd_tuple (_,y) = y

-- Now we construct an Add Tuples function
--  It outputs the sum of tuples in a list
addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [ x+y | (x,y) <- xs]

main =
  do

    -- Simples cases
    putStrLn ("List A: " ++ (show lst_a))
    putStrLn ("Range from 1 to 4: " ++ (show lst_out))
    putStrLn ("Head of the previous list: " ++ (show (head lst_out)))
    putStrLn ("Tail of the previous list: " ++ (show (tail lst_out)))
    putStrLn ("Length of the previous list: " ++ (show (length lst_out)))
    putStrLn ("Without the last element of the previous list: " ++ (show (init lst_out)))

    -- Pattern matching and custom functions
    putStrLn ("The doubled list is: " ++ (show [ 2*x | x <- [1, 2, 3, 4] ]))
    putStrLn ("The doubled list without small elements is: " ++ (show (double_and_cut lst_out)))
    putStrLn ("The tuple product of the list with itself is: " ++ show (tuple lst_out lst_out))
    putStrLn ("The sum of the list is: " ++ show (total lst_out))
    putStrLn ("The even elements are: " ++ show (evens lst_out))

    -- Tuples
    putStrLn ("The first element in the (0, 5) tuple is: " ++ show (fst_tuple (0, 5)))
    putStrLn ("The second element in the (9, 3) tuple is: " ++ show (snd_tuple (9, 3)))
    putStrLn ("The addition of these tuples with (4, 4) is: " ++ show (addTuples[(0, 5), (9, 3), (4, 4)]))

  where
    lst_out = (range 1 4)
