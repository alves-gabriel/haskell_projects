-- # Commands & Topics
-- Here we learn
--  >
--
-- # Hints:
--
-- # Refs
--  > https://www.youtube.com/watch?v=m12c99qgHBU
--  > https://stackoverflow.com/questions/6652234/how-does-currying-work
--  > https://wiki.haskell.org/Currying

main :: IO()

-- # Currying
--  It's the principle that
--  f :: a-> b -> c -> d
--  f :: a-> (b -> (c -> d))
--
--  in other words, this function of 4 arguments is just 4 nested functions (one which
--  takes a, returns another function, which takes b and returns another function, and so on
--  and so fort).
--
-- The function add inthe example below is equivalent to "add x y = x + y"
add :: Int -> Int -> Int
add = (\x -> (\y -> x + y))

-- # Partial function application
--  What would we get writing "add 1" in the previous function? Note that at its core
--  "add" returns a new function! Because first we pass x, then we get a new function, to
--  which we pass y. This new function is:
--  add 1 :: Int -> Int => (\y + 1 + y),
--  so we pass an argument (which would be the y, previously), and finally get a number
--  as an output.
--
--  In short:
--  This is a way of changing the behaviour of a function and generating a new function
--  from old functions. This is what we call partial function application.
--  Ex: map (a -> b) -> [a] -> [b]
--  Here we define a new function making use of the partial function application.
--  Note how this function does not need any arguments, since they are "implicitly"
--  defined on the right-hand side.
doubleList = map (\x -> 2*x)

-- Notice the comment Mirzhan Irkegulov's reply in the stack exchange question:
--
-- "That's why function application syntax in Haskell has no parentheses and commas,
-- compare Haskell's f a b c with Python's or Java's f(a, b, c). It's not some weird
--  aesthetic decision, in Haskell function application goes from left to right, so
-- f a b c is actually (((f a) b) c), which makes complete sense, once you know
-- that f is curried by default."

main =
  do
    putStrLn ("5 + 7 is " ++ (show (add 5 7)))
    putStrLn ("Doubled [1, 2, 3] is " ++ (show (doubleList [1, 2, 3])))
