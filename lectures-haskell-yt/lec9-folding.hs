-- # Commands & Topics
-- Here we learn
--  > Folding
--  > foldr
--  > foldl

-- # Hints: foldr is better for lazy evaluation. If one has to use foldl, then maybe foldl' is better: https://wiki.haskell.org/Foldr_Foldl_Foldl'
--
-- # Refs
--  > https://www.youtube.com/watch?v=0qvi_sTJbEw
--  > https://wiki.haskell.org/Fold
--  > https://stackoverflow.com/questions/13280159/haskell-foldl-and-foldr/13280185

main :: IO()

-- # Foldr (fold right)
--  foldr :: (a-> b -> b) -> b ->[a] -> b
--  Has one argument of type a and other of type b. Returns function of type b. It's then defined as:
--
--  ################################################
--  # foldr (⊕) a [x1,...,xn] = x1 ⊕ ... ⊕ xn ⊕ a  #
--  ################################################
--
--  So, in pratice a is the starting values (in some contexts it's called the accumulator)
--  Thus, foldr is just the combination of all the functions and its starting value.
--
--  For instance, we may have foldr (+) 0 [1,...,n] = 1 + 2 + ... + n + 0
--  Down below I'm using partial function application, but I could've written
--  total lst = foldr (+) 0 lst as well
total :: [Int] -> Int
total = foldr (+) 0

--   Another thing we can do is to construct the functions and, or and sum
--    sum = foldr (+) 0
--    and = foldr (&&) True
--    or = foldr (||) False

--  What if we want another function in foldr?  We may use the structure:

--  foldr (\elem acc -> <term>) <start_acc> <list>
--
--  For example, let's make a function which counts how many times and element appears in a list:
count :: Int -> [Int] -> Int
count el lst = foldr (\x acc -> if x==el then acc+1 else acc) 0 lst

--  This other function checks wheter all elements in a list match el
--  We could have also tried to do this with map.
isAll el = foldr (\x acc -> x==el && acc) True

-- # Folding
--  Other elementary functions are built upon folding.
--    length = foldr (\x -> (+) 1) 0 (one may also do foldr (const $ (+) 1) 0) to explicity ignore x
--    map f = foldr ((:) . f) []

-- # Foldl (folt left)
--  It's important to note here that the element and the accumulator are switched
--
--  foldr (\acc elem -> <term>) <start_acc> <list>
--
-- The difference between foldl and foldr is very important when the function is not associative.
--
--  For instance:
--
--  foldr (-) 0 [1, 2, 3]  = 1 - (2 - (3))) = 1 - (-1) = 2
--  foldl (-) 0 [1, 2, 3]  = ((0 - 1) - 2) - 3 = -6
--
--    From another site: "It takes the second argument and the first item of the list and
--    applies the function to them, then feeds the function with this result and the second
--    argument and so on. See scanl for intermediate results."
--    Ref: http://zvon.org/other/haskell/Outputprelude/foldl_f.html
--
--  Another example from the same link:

--  foldl (/) 64 [4,2,4]
--  foldl (/) 16 [4,2]
--  foldl (/) 8 [4]
--  foldl (/) 2 [] => 2
--
--  Or:
--
--  ################################################
--  # foldl (⊕) a [x1,...,xn] = a ⊕ x1 ⊕ ... ⊕ xn  #
--  ################################################
--
--  foldl (/) 64 [4,2,4]; here a = 64 and [x1, x2, x3] = [4, 2, 4], so
--  64 / 4 / 2 / 4 = 2
--  or
--  foldl (-) -1 [1, 2, 3];
--  (-1) ⊕ 1 ⊕ ... ⊕ 3, where ⊕ = (-)
--  (-1) (-) 1 (-) 2 (-) 3
--  ((-1) - 1) (-) 2 (-) 3
--  (((-1) - 1) - 2) (-) 3
--  ((((-1) - 1) - 2) - 3) = - 7
--
--  Compare this with;
--  foldr (-) -1 [1, 2, 3];
--  1 ⊕ ... ⊕ 3 ⊕ (-1), where ⊕ = (-)
--  1 (-) 2 (-) 3 (-) (-1)
--  1 (-) 2 (-) (3 - (-1))
--  1 (-) (2 - (3 - (-1)))
--  (1 - (2 - (3 - (-1)))) = 3

-- # Folding (tree)
--  What if we define it on a tree?
--  It can be in order, from left to right (sort like foldr), or in post-order, from right ot left
--  Or in pre-order, were we go in a zig-zag way (each level at a time).
--  No single answer! It depends on what you need/want.

main =
  do
    putStrLn ("The sum of [1, 2, 3, 4] is " ++ (show $ total  [1, 2, 3, 4]))
    putStrLn ("1 appears in [1, 2, 1, 1, 2] " ++ (show $ count 1 [1, 2, 1, 1, 2]) ++ " times" )
    putStrLn ("Is all 5 in [5, 5, 44]? " ++ (show $ isAll 5 [5, 5, 44]))
    putStrLn ("Is all 5 in [5, 5, 5]? " ++ (show $ isAll 5 [5, 5, 5]))
    putStrLn ("The code foldl (/) 64 [4,2,4] yields " ++ (show $ foldl (/) 64 [4,2,4]))
    putStrLn ("The code foldl (-) -1 [1,2,3] yields " ++ (show $ foldl (-) (-1) [1,2,3]))
    putStrLn ("The code foldr (-) -1 [1,2,3] yields " ++ (show $ foldr (-) (-1) [1,2,3]))
