-- Since splitAt yields a tuple (x, y), we use pattern matching here to invert
-- its order, i.e., we return y concatenated with x.
--
-- mod n $ length lst is used to handle the cases where n < 0 or n > length of lst
rotateLeft :: [a] -> Int -> [a]
rotateLeft lst n = (\(x,y) -> y++x) (splitAt (mod n (length lst)) lst)

{-
*Main> rotateLeft [1, 2, 3, 4, 5] 0
[1,2,3,4,5]
*Main> rotateLeft [1, 2, 3, 4, 5] 1
[2,3,4,5,1]
*Main> rotateLeft [1, 2, 3, 4, 5] 2
[3,4,5,1,2]
*Main> rotateLeft [1, 2, 3, 4, 5] 5
[1,2,3,4,5]
*Main> rotateLeft [1, 2, 3, 4, 5] $ -2
[4,5,1,2,3]
*Main> rotateLeft [1, 2, 3, 4, 5] 8
[4,5,1,2,3]
}
