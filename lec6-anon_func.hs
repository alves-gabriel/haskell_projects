-- # Commands & Topics
-- Here we learn
--  > Higher order functions
--  > Anonymous functions
--  > Map
--
-- # Hints:
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

-- # Anonymous functions
--  This is a function whose name we don't want to define (speciially) in this
--  higher order functions contexts. The general structure is:
--  (\<args> -> <expr>)
f_succ_anon = (\x -> x +1)
total_anon = (\x y z -> x+y+z)

-- The nice thing is to combine both, e.g.
-- app (\x -> x + 1) 1 => 2

-- # Map
--  It maps one list of type a into another list of type b. The first argument is
--  used to perform this conversion.
--
--  map :: (a -> b) [a] -> [b]
--   [a, b, ...,  y,  z]
--   |  |        |   |
--   v  v        v   v
--  [1, 2, ..., n-1, n]
--
-- Function which increaments each element in a list
list_increment lst = map (\x -> x + 1) lst

-- Function which sums the tuples in a list
tpl_sum lst = map (\(x, y) -> x + y) lst

-- # Filter
--  It takes a function as its argument (which yields a bool), and returns a list of
--  the same type with element satisfying the given condition
--  filter :: (a-> Bool) -> [a] -> [a]
delete_doubles lst = filter (\(x, y) -> x/=y) lst

main =
  do
    putStrLn ("The successor of 5 is " ++ (show (app f_succ 5)))
    putStrLn ("The successor of 44 is " ++ (show (app f_succ 44)))
    putStrLn ("The total of 5, 44 and 7 is " ++ (show (total_anon 5 44 7)))
    putStrLn ("The increment of [5, 44, 7] is " ++ (show (list_increment [5, 44, 7])))
    putStrLn ("The sum of the tuples [(5, 44), (7, 14)] is " ++ (show (tpl_sum [(5, 44), (7, 14)])))
    putStrLn ("The list [(5, 44), (7, 7),(14, 63)] without doubles is " ++ (show (delete_doubles [(5, 44), (7, 7),(14, 63)])))
