-- # Commands & Topics
-- Here we learn
--  > Defining datatypes
--
-- # Hints:
--
-- # Refs
--  > https://www.youtube.com/watch?v=7sbxVALuuxA
--  > https://stackoverflow.com/questions/47269879/how-to-show-a-user-defined-data-type-in-haskell
--  > http://learnyouahaskell.com/making-our-own-types-and-typeclasses
--
--  Copy template with: cat template.hs >> "New file"

main :: IO()

-- # Datatypes
--  data Name = Constructor1 <args> | Constructor2 <args> | ...
--
--  The name HAS to start with a capital letter. Then we supply constructors (which
--  can have arguments).
--
--  Examples:
--
--  data Color = Red | Orange | Yellow | Green | Blue | Magenta
--  data NaturalNumber = Succ NaturalNumber | Zero (it's either zero or a recursevely defined number, coming from Succ)
--  data Calculation = Add Int Int | Sub Int Int | Mul Int Int | Div Int Int
--
--
--  This is equivalent to writing:
--    Red :: Color
--    Orange :: Color... etc
--
--  If we type ":type Red" in ghci we get
--
--  *Main> data Color = Red | Orange | Yellow | Green
--  *Main> :type Red
--
--  This is pretty useful!
--
--  Our own constructors, can, for instance be used in a pattern matching (just like it was done with lists)
--  (The lists had two constructors: the :/prepend constructor and the empty list.

data Calculation = Add Int Int | Sub Int Int | Mul Int Int | Div Int Int
calc :: Calculation -> Int
calc (Add x y) = x+y
calc (Sub x y) = x-y
calc (Mul x y) = x*y
calc (Div x y) = div x y

--  Now, coming back to recursive data types.
data NaturalNumber = Succ NaturalNumber | Zero
four :: NaturalNumber
four = Succ $ Succ $ Succ $ Succ $ Zero

--  Note that the Tree data type we define here now has a polymorphic data type in it.
--  The definition here works as follows: we have either a leaf or a node, that has some value
--  in it's middle and then a left and right subtree. I.e. (Left Subtree) Value (Rightsubtree)
data Tree a = Leaf | Node (Tree a) a (Tree a)
tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 Leaf)

-- # Working with the data types
--  Note that partial function application is in use here! :)
incr :: NaturalNumber -> NaturalNumber
incr = Succ

--  Now a couple more examples
decr :: NaturalNumber -> NaturalNumber
decr (Succ n) =  n

natural_add :: NaturalNumber -> NaturalNumber -> NaturalNumber
natural_add Zero n      = n
natural_add (Succ m) n  = Succ $ natural_add m n

natural_sum :: [NaturalNumber] -> NaturalNumber
natural_sum []      = Zero
natural_sum (x:xs)  = natural_add x $ natural_sum xs

--  Check https://stackoverflow.com/questions/47269879/how-to-show-a-user-defined-data-type-in-haskell
--  on how to print custom datatypes.
natural_show :: NaturalNumber -> String
natural_show Zero = "Zero"
natural_show (Succ k) = "Succ " ++ (natural_show k)

main =
  do
    putStrLn (natural_show four)
    putStrLn (natural_show (natural_add four four))
    putStrLn (natural_show (natural_sum [four, four, four]))
