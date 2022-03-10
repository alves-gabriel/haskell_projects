import System.Random
--
-- replace :: Int -> a ->  [a] ->  [a]
-- replace _ _ [] = []
-- replace n x' (x:xs)
--   | n > 0     = x : replace (n - 1) x' xs
--   | otherwise = x':xs

-- swap ::  [a] -> Int -> Int ->  [a]
-- swap lst m n =  replace n (lst!!m) (replace m (lst!!n) lst)

-- randomList :: Int -> Int -> Int -> ([Int])
-- randomList 0 _ _ = return []
-- randomList rLength m n = do
--   r  <- randomR (m, n)
--   rs <- randomList (rLength - 1) m n
--   return (r:rs)
