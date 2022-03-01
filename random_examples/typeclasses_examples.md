# Type classes examples

### Sorting in a suit of cards

Here we show an example where type classes can, among many other things, be used to cleverly sort the suits in a deck of cards. Initially we define the suit as a new data type

```haskell
data Suit = Diamond | Spade | Heart | Club
  deriving (Eq, Ord, Enum)
```

from which we would like to derive ```Eq, Ord``` and ```Enum``` typeclasses. This follows the sorting of the Brazilian card game _Truco_, namely ♦ < ♠ < ♥ < ♣:

 ```
*Main> Club > Diamond
True
*Main> Spade > Heart
False
 ```

How can we construct a sorting algorithm for this data type? A possible approach is the following:

```haskell
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
```

In ```aux_sort``` we perform a single iteration of the Bubble sort algorithm. In this step we go through the list once, comparing pair of elements and swapping then if necessary (note how in the first iteration we move the largest element to the end, but the rest of the list remains unsorted):

```
*Main> suit_lst = [Club, Spade, Diamond, Heart]
*Main> aux_sort suit_lst
[♠,♦,♥,♣]
```

 Given that the list is sorted if (and only if) it does not change between two subsequent iterations, then in ```suit_sort``` we can just check (recursively) whether the list changed or not. If the list did not change then the list is already sorted, and we return it. Otherwise, we just further apply another ```aux_sort``` step recursively.

```
*Main> suit_sort suit_lst
[♦,♠,♥,♣]
```

### Record syntax

Records are particularly useful when we intend to use data types whose value constructors contain several fields. Let's start by creating a data type called ```Point```, which can be either two or three-dimensional:

```Haskell
data Point =
     TwoD { x0 :: Float, y0 :: Float}
   | ThreeD { x1 :: Float, y1 :: Float, z1 :: Float}
```

Note the use of curly brackets here. It's also desirable to give different name to the fields inside the brackets. Checking the type of both the value constructors and its fields will give us some hints on how they should be used:

```
*Main> :t x0
x0 :: Point -> Float
*Main> :t TwoD
TwoD :: Float -> Float -> Point
```
We can for instance, write

```
*Main> x0 TwoD {x0 = 1, y0 = 2.5}
1.0
```

to recover the fields. Calling the point itself however has no effects. It actually raises an error:

```
*Main> TwoD {x0 = 1, y0 = 2.5}

<interactive>:12:1: error:
    • No instance for (Show Point) arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it
```

If we establish an instance for show, so the function works with this data type,

```Haskell
instance Show Point where
  show (TwoD x0 y0) = "(x = " ++ (show $ x0) ++ ", y = " ++ (show $ y0) ++ ")"
  show (ThreeD x1 y1 z1) = "(x = " ++ (show $ x1) ++ ", y = " ++ (show $ y1) ++ ", z = " ++ (show $ z1) ++ ")"
```

we get:

```
*Main> TwoD {x0 = 1, y0 = 2.5}
(x = 1.0, y = 2.5)
```

We can also construct functions which have ```Point``` types as an input. We can also conveniently ignore the irrelevant parameters on functions which do not require all of them. In the following example we apply this to a cartesian-spherical coordinates conversion (see the function ```theta```):

```haskell
radius :: Point -> Float
radius (ThreeD x y z) = sqrt(x**2 + y**2 + z**2)

theta :: Point -> Float
theta (ThreeD x y _) = atan (y/x)

phi :: Point -> Float
phi (ThreeD x y z) = acos (z/ radius (ThreeD x y z))
```

We can call this functions in the following way, for example:

<<<<<<< HEAD
```
=======
```haskell
>>>>>>> eff0dddf7e04a280918368fba702bcccc05a5255
*Main> let coordinate1 = ThreeD {x1 = 0, y1 = -1, z1 = 3}
*Main> radius coordinate1
3.1622777
*Main> theta coordinate1
-1.5707964
```

We may also define binary operations between points, which can yield another `Point` type, a `Float` type or any other sensible data type.

```haskell
vecAdd :: Point -> Point -> Point
vecAdd (ThreeD c0 c1 c2) (ThreeD c0' c1' c2') = ThreeD {x1 = c0 + c0', y1 = c1 + c1', z1 = c2 + c2'}

dotProd :: Point -> Point -> Float
dotProd (ThreeD c0 c1 c2) (ThreeD c0' c1' c2') = c0*c0' + c1*c1' + c2*c2'
```

Example usage:

<<<<<<< HEAD
```
=======
```Haskell
>>>>>>> eff0dddf7e04a280918368fba702bcccc05a5255
*Main> let coordinate1 = ThreeD {x1 = 0, y1 = -1, z1 = 3}
*Main> let coordinate2 = ThreeD {x1 = 1, y1 =  1, z1 = 2}
*Main> coordinate1 `vecAdd` coordinate2
(x = 1.0, y = 0.0, z = 5.0)
*Main> coordinate1 `dotProd` coordinate2
5.0
```
