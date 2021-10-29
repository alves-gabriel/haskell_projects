-- # Exercise
--  Create a program which checks wether a certain path in a DIRECTED graph
-- exists between to vertices A and B .
--
-- # Refs
--  > https://www.youtube.com/watch?v=Cxkqrg8FCt8

main :: IO()

-- Test case
path = [(1,2),(2,3),(3,2),(4,3),(4,5)]

-- pathQ return True if a path is found and False otherwise
pathQ :: [(Int, Int)] -> Int -> Int -> Bool
pathQ graph a b
  | a==b = True
  | otherwise =
      -- Evaluates the function for all the edges which arrive on b from y but
      -- leave from (a,y)
      or [pathQ compPath y b | (x,y) <- graph, x == a]
      where
        -- Takes all the edges where a is NOT the source. I.e., constructs a
        -- "complementary" space with all edges which do not begin on a
        compPath = [(x,y) | (x,y) <- graph, x/=a ]

-- pathQ  [(1,2),(2,3),(3,2),(4,3),(4,5)] 1 3
-- 1st step
-- a = 1 b = 3
-- compPath = [(2,3),(3,2),(4,3),(4,5)]
-- or [pathQ [(2,3),(3,2),(4,3),(4,5)] y b, such that x == a], e.g.
-- If we call [(2,3),(3,2),(4,3),(4,5)] = G1, then
-- or [pathQ G1 y 3 | (x, y) <- graph, x==1], so we are looking for paths
-- in G1 which leave from y and arrive on 3. For  (x, y) | x==1 we have (x,y) = (1,2)
-- so the previous line becomes
-- or [pathQ G1 2 3]
-- And this time G2 = compPath = [(1,2),(3,2),(4,3),(4,5)]
-- so we look for or [pathQ G2 y 3 | (x, y) <- graph, x==2]
-- and the tuple which satisfies x==2 is only (2,3) so we get
-- or [pathQ G1 3 3] = or True = True, since a = 3 = b

main =
  do
    putStrLn ("Is 1 -> 4 a valid path? " ++ (show (pathQ path 1 4)))
    putStrLn ("Is 1 -> 3 a valid path? " ++ (show (pathQ path 1 3)))
    putStrLn ("Is 4 -> 2 a valid path? " ++ (show (pathQ path 4 2)))
    putStrLn ("Is 3 -> 1 a valid path? " ++ (show (pathQ path 3 1)))
    putStrLn ("Is 4 -> 1 a valid path? " ++ (show (pathQ path 4 1)))
    putStrLn ("Is 5 -> 4 a valid path? " ++ (show (pathQ path 5 4)))
