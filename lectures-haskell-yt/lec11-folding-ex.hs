-- # Commands & Topics
-- Here we learn
--  >
--
-- # Hints:
--
-- # Refs
--  >
--
--  Copy template with: cat template.hs >> "New file"

main :: IO()

-- # Exercise 1
--  Create a funciton rev that reverses a list
--  Also check: https://stackoverflow.com/questions/11569650/what-is-the-difference-between-and-in-haskell
rev :: [a] -> [a]
rev []=[]
rev (x:xs) = (rev xs)++[x]

--  A possible implementation using fold is the one shown below.
--  Here x_rest plays the role of the accumulator.
--  Here, if lst = [x1,...,xn] we are doing:
--  foldl ⊕ [] [x1,...,xn], where ⊕ = (\x_rest x -> x : x_rest).
--  For simplicity let's take n = 3. We have
--  foldl ⊕ [] [x1, x2, x3]
--  [] ⊕ x1 ⊕ x2 ⊕ x3
--  ([] : x1) ⊕ x2 ⊕ x3
--  [x1] ⊕ x2 ⊕ x3
--  (x2 :[x1]) ⊕ x3
--  [x2, x1] ⊕ x3
--  x3:[x2, x1]
--  [x3, x2, x1]
rev2 lst = foldl (\x_rest x -> x : x_rest) [] lst

-- # Exercise 2
--  Create a function prefixes which return all the prefixes of a given list

-- My solution. Not so great, kinda imperative (?)
prefixes lst = foldl (\y x-> ( y++((head $ rev y)++[x]):[] ) ) [[head lst]] (tail lst)

main =
  do
    putStrLn ("The reverse of [1,2,3] is " ++ (show $ rev [1, 2, 3]))
    putStrLn ("The reverse of [4,5,6] is " ++ (show $ rev2 [4, 5, 6]))
    putStrLn ("The prefixes of [1,2,3] are " ++ (show $ prefixes [1,2,3] ))
