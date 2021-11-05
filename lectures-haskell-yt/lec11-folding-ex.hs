-- # Commands & Topics
-- Here we learn
--  > Foldl and foldr practice
--
-- # Hints:
--
-- # Refs
--  > https://www.youtube.com/watch?v=7sbxVALuuxA
--  > https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List.html
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
prefixes :: [a] -> [[a]]
prefixes lst = foldl (\y x->  y ++ ((head $ rev y)++[x]):[]  ) [[head lst]] (tail lst)

-- Solution from the video. We go through the list in reverse and always prepend
-- the element we are at. So we always put the ending value x to the accumulator.
-- So we build all the prefixes from the last element to the first element.
-- For ex:  ([2] : map ((:) 2) [[3]]) = [[2], [2,3]]
--
-- Since foldr ⊕ [x1, ..., xn] = x1 ⊕ ... ⊕ xn ⊕ a
--
-- 1 ⊕ 2 ⊕ 3 ⊕ []
-- 1 ⊕ 2 ⊕ ([3] : map ((:) [3]) [])
-- 1 ⊕ 2 ⊕ [[3]]
-- 1 ⊕ ([2] : map ((:) 2) [[3]]) = [[2], [2,3]]
-- 1 ⊕ [[2], [2,3]]
-- ([1] : map ((:) 1) [[2], [2,3]])
-- [[1], [1,2], [1,2,3]]
--
-- In more detail:
--
--   Prepends the current element x as [x]
--             |
-- (\x acc -> [x]  : (map ((:) x) acc))
--    |                      |
-- x: element on left      Prepends x to every prefix on the right. E.g.
-- acc: partial list       map ((:) 1) [[2], [2,3]] prepends 1 to [2] and then to
-- of prefixes on right    [2, 3]

prefixes2 :: [a] -> [[a]]
prefixes2 = foldr (\x acc -> [x]  : (map ((:) x) acc)) []

-- # Exercise 3
--  Construct a function which receives a set of k data points (xk, yk) and a point x
-- and returns L(x), the interpolated Lagrangian polynomial at x:
-- https://en.wikipedia.org/wiki/Lagrange_polynomial
--
-- Test case:
-- f = lagrange [(1.0, 1.0), (2.0, 4.0), (3.0, 9.0)]
-- f 5.0 => 25.0
--
-- The strategy here is to first use a fold for lj(x) and then another one for the sum in L(x).
--
-- Fold is a natural way to construct summations and products of several terms, so. For the products
-- (x - xm)/(xj - xm) for j!=m we can use foldl. Note that this can be written as:
--
-- (x - x0)/(xj - x0)...(x - xn)/(xj - xn) x 1, so since foldr (⊕) a [x1,...,xn] = x1 ⊕ ... ⊕ xn ⊕ a
-- it's not hard to identify the base case/initial value for the accumulator as a = 1.
-- No, regarding the binary operation (⊕), note that if we have
--
-- x1 ⊕ ... xm ⊕ acc
--
-- It's possible to identify the accumulator acc as all the products to the right of xm, so
-- acc = (x - x(m+1))/(xj - x(m+1))...(x - xn)/(xj - xn)
-- so what the binary operation should do is:
--
-- xm ⊕ acc = (x - xm)/(xj - xm) * acc
--
-- we just construct the m-th term in the product as  (x - xm)/(xj - xm), as long xm != xj and then
-- multiply it by all the previous products, which are contained within acc. Note the use of pattern
-- matching in the implementation. We do \(xm, _) acc  when defining the anonymous function.
-- If xj = xm then we want to do nothing. The proper way to do this then is to return the accumulator
-- itself, i.e.
--
-- xm ⊕ acc = acc, for xj = xm
--
-- The reasoning is quite similar for the summation. But in this case a = 0, and instead of multiplying
-- by the accumulator we simply sum. So
--
-- (xj, yj) ⊕ acc = yj*l_j(x) + acc

-- lj xj xy_lst x :
--  Returns the value l_j(x), i.e. the j-th term in the lagrangian polynomial
--
-- xy_lst : tuples [(x, y)] of points
-- x: point x to be evaluated
-- xj: x values of the j-th point in the tuples
lj :: Float -> [(Float, Float)] -> Float -> Float
lj xj xy_lst x = foldr (\(xm, _) acc -> if xm /= xj then ((x - xm)/(xj - xm))*acc else acc) 1.0 xy_lst

lagrange :: [(Float, Float)] -> Float -> Float
lagrange xy_lst x = foldr (\(xj, yj) acc ->  yj*(lj xj xy_lst x) + acc) 0.0 xy_lst

main =
  do
    putStrLn ("The reverse of [1,2,3] is " ++ (show $ rev [1, 2, 3]))
    putStrLn ("The reverse of [4,5,6] is " ++ (show $ rev2 [4, 5, 6]))
    putStrLn ("The prefixes of [1,2,3] are " ++ (show $ prefixes [1,2,3] ))
    putStrLn ("The prefixes of [1,2,3,4] are " ++ (show $ prefixes [1,2,3,4] ))
    putStrLn ("l_1(4) of [2, -1, 0] is " ++ (show $ (lj 2 [(2, 0), (-1, 3), (0, 1)] 7) ))
    putStrLn ("L(5) for the points [(1.0, 1.0), (2.0, 4.0), (3.0, 9.0)] is " ++ (show $ lagrange [(1.0, 1.0), (2.0, 4.0), (3.0, 9.0)] 5.0))
    putStrLn ("L(-7) for the points [(1.0, 1.0), (2.0, 4.0), (3.0, 9.0)] is " ++ (show $ lagrange [(1.0, 1.0), (2.0, 4.0), (3.0, 9.0)] (-7.0)))
