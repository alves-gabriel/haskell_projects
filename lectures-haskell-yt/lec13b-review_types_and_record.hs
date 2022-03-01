-- # Commands & Topics
-- Here we learn
--  > Typeclasses
--
-- # Hints:
--
-- # Refs
--  > http://learnyouahaskell.com/making-our-own-types-and-typeclasses
--  > https://en.wikibooks.org/wiki/Haskell/Classes_and_types
--  > https://www.youtube.com/watch?v=CoNZRKqQi1o
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

-- We define coordinates as the data type "Point". This data type can be either
-- two-dimensional or three dimensional i.e. its value constructors
data Point =
     TwoD { x0 :: Float, y0 :: Float}
   | ThreeD { x1 :: Float, y1 :: Float, z1 :: Float}

-- Here we construct an instance for how "show" should operate on Point types
instance Show Point where
  show (TwoD x0 y0) = "(x = " ++ (show $ x0) ++ ", y = " ++ (show $ y0) ++ ")"
  show (ThreeD x1 y1 z1) = "(x = " ++ (show $ x1) ++ ", y = " ++ (show $ y1) ++ ", z = " ++ (show $ z1) ++ ")"

-- Functions which output the spherical components of this point
radius :: Point -> Float
radius (ThreeD x y z) = sqrt(x**2 + y**2 + z**2)

theta :: Point -> Float
theta (ThreeD x y _) = atan (y/x)

phi :: Point -> Float
phi (ThreeD x y z) = acos (z/ radius (ThreeD x y z))

-- Binary operations for Point data types. We implement the vector addition
-- and the dot product. Note how in the first function we have to Points as
-- inputs and also a Point as output, since we get a vector when we add two vectors.
-- For the second function, however, we have Points as inputs but a Float as an
-- output.
vecAdd :: Point -> Point -> Point
vecAdd (ThreeD c0 c1 c2) (ThreeD c0' c1' c2') = ThreeD {x1 = c0 + c0', y1 = c1 + c1', z1 = c2 + c2'}

dotProd :: Point -> Point -> Float
dotProd (ThreeD c0 c1 c2) (ThreeD c0' c1' c2') = c0*c0' + c1*c1' + c2*c2'
