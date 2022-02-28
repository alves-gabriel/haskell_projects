-- # Commands & Topics
-- Here we learn
--  > Typeclasses
--
-- # Hints:
--
-- # Refs
--  > https://www.youtube.com/watch?v=1txgSlcpQmo
--  > http://learnyouahaskell.com/making-our-own-types-and-typeclasses
--  > https://en.wikibooks.org/wiki/Haskell/Classes_and_types
--
--  Copy template with: cat template.hs >> "New file"

main :: IO()

--  Take a function such as (+): we want it to be polymorphic up to a certain
--  degree. What we do not want, however, is for this function to be totally
--  polymorphic, e.g. we want to be able to add intergers and floats, but not
--  other exotic types, such as trees. Example:
--
--    (+) :: Num a => a -> a -> a
--
--  "Num a" here is a type constraint. This means that (+) works for nay polymorphic
--  type which has an instance of the Num typeclass. Typeclass defines some functions that
--  need to be defined in a class. And for the type to be in that type class it has
--  to have an isntance of such a class. When you use (+) in another function,
--  this type constraint is propagated.
--
--  To find out how the num type class looks like it suffices to type ":info" into ghci
--
-- Prelude> :info Num
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--
--  We get information on what functions are defined for types that are in this type class.
--  Another interesting typeclass is Eq (used for checking)
--
-- Prelude> :info Eq
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}
--   	-- Defined in ‘GHC.Classes’
--
-- And for ordered:
--
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a
--   {-# MINIMAL compare | (<=) #-}
--
-- Note how Eq implies Ord: namely, in order for a type to be Ord, it must also
-- be Eq i.e. anything that can be ordered can also be checked.

-- Here we have an example for the data type "Temperature", which stores it
-- either in Celsius or Fahrenheit. How do we compare them?
data Temperature = C Float | F Float

-- We create an instance of the Eq type class for this temperature. See how
-- not equal is not defined, but rather it is derived/implictly defined
-- (This MAY not working because of rounding issues, but that's fine for the sake of the example)
instance Eq Temperature where
  (==) (C n) (C m) = n == m
  (==) (F n) (F m) = n == m
  (==) (C c) (F f) = (9*c/5 + 32) == f
  (==) (F f) (C c) = (9*c/5 + 32) == f

-- If we wanted to define Temperature as the Num typeclass then we would need to define much more functions
--
-- We can also derive typeclasses. For instance, how can we make this printable?
-- We don't need to write new instances for the Show typeclass all the time. We can do:
--
-- data Temperature = C Float | F Float
--   deriving(Show, Eq)
--
-- Look for derived equivalence later on! (Usually fine for lists, for trees it depends, and for temperature it does not work)
-- So it should makes sense how Haskell compares this structurally. This is ok for show (possibly), but not for EQ in the case
-- of the temperature.

main =
  do
    putStrLn (" " ++ (show ( )))
