main :: IO ()

in_range min max x =
  x >=min && x<= max

my_var :: Integer
my_var = 44

out_range :: Integer -> Integer -> Integer -> Bool
out_range min max x =
  not (in_range min max x)

main = do
    print (in_range 0 5 3)
    print (out_range 0 5 3)
    print (out_range 0 5 6)
