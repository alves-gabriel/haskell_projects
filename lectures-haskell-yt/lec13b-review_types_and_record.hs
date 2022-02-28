-- # Commands & Topics
-- Here we learn
--  > Typeclasses
--
-- # Hints:
--
-- # Refs
--  > http://learnyouahaskell.com/making-our-own-types-and-typeclasses
--  > https://en.wikibooks.org/wiki/Haskell/Classes_and_types
--
--  Copy template with: cat template.hs >> "New file"

-- Type classes for suits and cards

data Suit = Diamond | Spade | Heart | Club
  deriving (Eq, Ord, Enum)

data Card = Card { number :: Int, suit :: Suit}

-- A single step in Bubble sort
aux_sort :: [Suit] -> [Suit]
aux_sort (x:y:xs)
  | x > y = y : aux_sort (x:xs)
  | otherwise = x : aux_sort (y:xs)
aux_sort (x) = x

-- Full bubble sort algorithm
suit_sort :: [Suit] -> [Suit]
suit_sort lst = if lst == aux_sort lst
  then lst
  else suit_sort $ aux_sort lst

instance Show Suit where
 show Diamond = "♦"
 show Spade = "♠"
 show Heart = "♥"
 show Club = "♣"
