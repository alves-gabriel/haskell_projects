-- # Commands & Topics
-- Here we learn
--  > Maybe
--
-- # Hints:
--
-- # Refs
--  > https://www.youtube.com/watch?v=O0iohEXMCsU
--  > https://wiki.haskell.org/Maybe
--  > https://wiki.haskell.org/Error_vs._Exception
--  > https://stackoverflow.com/questions/43018227/understanding-maybe-in-haskell
--
--  Copy template with: cat template.hs >> "New file"

main :: IO()

-- Here we discuss the Maybe data type
--
--  Given a function f x, what should it returns in the case of an error/exception?
--
--          f  x
--         / \
--        /  \
--    Result Error (?)
--
--  E.g., what is the head of []? Nothing, we get an exception.
--
--  data Maybe a = Nothing | Just a
--
--  Let's construct a safe division function to investigate this (note the binding =>)
safediv :: Integral a => a -> a -> Maybe a
safediv a b =
  if b == 0 then Nothing else Just $ div a b

--  On GHCI:
--
-- *Main> safediv 30 5
-- Just 6
-- *Main> safediv 44 0
-- Nothing
-- *Main>
--
-- We can use import Data.Maybe to borrow some functions for this data type
--
--  E.g., isJust :: Maybe a -> Bool, isNothing :: Maybe a -> Bool, or fromJust :: Maybe a -> a

main =
  do
    putStrLn (" " ++ (show ( )))
