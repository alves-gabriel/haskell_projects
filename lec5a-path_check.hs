-- # Exercise
--  Create a program which checks wether a certain path in a DIRECTED graph
--  exists between two vertices A and B .
--
-- # Refs
--  > https://www.youtube.com/watch?v=Cxkqrg8FCt8

main :: IO()

-- Test case (a bit different from the YT video)
--
-- 1 -> 2 <-> 3
--      |     |
--      v     v
--      5     4
--
path = [(1,2),(2,3),(2,5),(3,2),(3,4),(5,6)]

-- # Solution
--  pathQ return True if a path is found and False otherwise
--  Important note on implementation: for Haskell "or [] = False", so
--  we do not need to implement this base case.
--
pathQ :: [(Int, Int)] -> Int -> Int -> Bool
pathQ graph a b
  | a==b = True
  | otherwise =
      -- Look for all the edges (a, y), i.e., all the vertices where one
      -- may directly arrive from a, in other words, we look for the neighbors
      -- of a.
      -- Now, recursively call pathQ compPath y b, i.e., the algorithm
      -- tries to reach b from all the neighbors from a.
      -- Note that if no edges are found in this comprehensive list, i.e.,
      -- if no edges in compPath leave from y to another vertex, then this comprehensive
      -- line yields "or [] = False". So the search can't proceed anymore and it returns False.
      or [pathQ compPath y b | (x,y) <- graph, x == a]
      where
        -- Takes all the edges where a is NOT the source. I.e., constructs a
        -- "complementary" space with all edges which do not begin on a.
        -- By doing so we are effectively removing already visited edges, and
        -- only edges to be visited remain on compPath.
        compPath = [(x,y) | (x,y) <- graph, x/=a ]

-- Results
main =
  do
    putStrLn ("Is 1 -> 4 a valid path? " ++ (show (pathQ path 1 4)))
    putStrLn ("Is 1 -> 3 a valid path? " ++ (show (pathQ path 1 3)))
    putStrLn ("Is 1 -> 5 a valid path? " ++ (show (pathQ path 1 5)))
    putStrLn ("Is 4 -> 1 a valid path? " ++ (show (pathQ path 4 1)))
    putStrLn ("Is 5 -> 4 a valid path? " ++ (show (pathQ path 5 4)))
