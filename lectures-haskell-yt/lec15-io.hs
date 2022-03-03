-- # Commands & Topics
-- Here we learn
--  > IO Monad
--  > Do notation (kinda)
--
-- # Hints:
--
-- # Refs
--  > https://www.youtube.com/watch?v=fP0srOQVGB8
--
--  Copy template with: cat template.hs >> "New file"

main :: IO()

-- The do notation performs strict evaluation:
greet :: IO ()
greet = do
  putStrLn "What is your name?"
  name <- getLine                         -- getLine is an IO action. It reads one line. This is not function application nor definition
  putStrLn ("Hello, " ++ name ++ ".")

-- How can we loop over IO? Recursion is also the solution here! As an example:
count :: Int -> Int -> IO ()
count n m = do
  putStrLn $ show n
  if n < m then
    count (n + 1) m
  else
    return ()

main =
  do
    greet
    count 2 5
