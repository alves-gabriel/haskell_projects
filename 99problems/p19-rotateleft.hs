-- Since splitAt yields a tuple (x, y), we use pattern matching here to invert
-- its order, i.e., we return y concatenated with x.
rotateLeft :: [a] -> Int -> [a]
rotateLeft lst n = (\(x,y) -> y++x) (splitAt n lst)

-- *Main> rotateLeft [1, 2, 3, 4, 5] 0
-- [1,2,3,4,5]
-- *Main> rotateLeft [1, 2, 3, 4, 5] 1
-- [2,3,4,5,1]
-- *Main> rotateLeft [1, 2, 3, 4, 5] 2
-- [3,4,5,1,2]
-- *Main> rotateLeft [1, 2, 3, 4, 5] 5
-- [1,2,3,4,5]
