-- Hints: functinons can't begin with uppercase letters
-- functions w/ no arguments are efinitions

main :: IO ()

-- Definition
def_function = "I'm a definition"

-- Exponentiation
power x y = x**y

-- Prints several different stuff. Do is required here, we can't define main twice
-- To print floating points by themselves we could just do print (power 2 3)
-- Show converts the interger to a string
main = do
    putStrLn "Write your name..."
    name <- getLine
    putStrLn (def_function++ " executed by " ++ name )
    putStrLn ("I also calculated 2^3=" ++ show (power 2 3) )
