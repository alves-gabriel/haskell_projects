main :: IO ()

-- Beware: a `div` b only works with interger
-- If one is interested in working with Float -> Float -> Maybe Float
-- then the last condition should be changed to Just(n/m)
safe_div :: Integer -> Integer -> Maybe Integer
safe_div n 0 = Nothing
safe_div n m = Just (n `div` m)

-- eval :: Expr -> Maybe Integer
-- eval a = a

main = do
  putStrLn ("How much is 8:3? It is " ++ (show (safe_div 8 3)))
  putStrLn ("How much is 8:0? It is " ++ (show (safe_div 8 0)))
