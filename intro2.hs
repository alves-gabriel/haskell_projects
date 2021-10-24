-- # Commands
-- Here we learn
--  > Function definitions
--  > Explicit type declaration
--  > Let and where bindings
--  > if and else constructions
--  > type Checking
--  > infix notation

-- :t var or :t function retuns the type in GHCI, e.g.
--    :t 9 returns 9 :: Num p => p
--    :t (+) returns (+) :: Num a => a -> a -> a
--    :t out_range returns out_range :: Integer -> Integer -> Integer -> Bool

-- # Refs
--  > https://www.youtube.com/watch?v=y6xiaSkVlvs

-- ##############
-- ## Fuctions ##
-- ##############

main :: IO ()

-- We define functions as
in_range min max x =
  x >=min && x<= max

-- We can explictly declare types. This also works for "constants", e.g.
-- my_var :: Integer
-- my_var = 44
out_range :: Integer -> Integer -> Integer -> Bool
out_range min max x =
  not (in_range min max x)

-- Let bindings. We "locally store" a value in an "imperative way"
in_range_binded min max x =
  let in_lower = min <= x
      in_uppper = max >= x
  in
  in_lower&&in_uppper

-- Where bindings
out_range_binded min max x = in_lower&&in_uppper
  where
    in_lower = min <= x
    in_uppper = max >= x

-- If/else constructions
if_in_range min max x =
  if in_lower then in_uppper else False
  where
    in_lower = min <= x
    in_uppper = max >= x

-- Functions can also be used in infix notation
-- e.g. 44 `sub` 5
sub a b = a-b

-- ###############
-- ### Outputs ###
-- ###############

main = do
  putStrLn "Is 3 between 0 and 5?"
  print (in_range 0 5 3)

  putStrLn "Is 3 outside 0 and 5?"
  print (out_range 0 5 3)

  putStrLn "Checking 44 between 0 - 5 with let bindings:"
  print (in_range_binded 0 5 44)

  putStrLn "Checking 7 outside 0 - 5 with where bindings:"
  print (out_range 0 5 7)

  putStrLn "Checking 33 between 0 - 5 with if/else bindings:"
  print (if_in_range 0 5 33)

  putStrLn "Calculating 44-5 through infix 44 `sub` 5:"
  print (44 `sub` 5)
