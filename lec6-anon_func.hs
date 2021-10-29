-- # Commands
-- Here we learn
--  > Higher order functions
--  > Anonymous functions
--
-- # Hint:
--
-- # Refs
--  > https://www.youtube.com/watch?v=ccExc6rrUN8

main :: IO()

-- # Higher order function
--  This a function which takes a function as an argument
app :: (a -> b) -> a -> b
app f x = f x

f_succ :: Int -> Int
f_succ x = x+1
app f_succ 1
