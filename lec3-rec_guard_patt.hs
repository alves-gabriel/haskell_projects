-- # Commands
-- Here we learn
--  > Recursion
--  > Guards
--  > Pattern matching
--  > Tail recursion

-- # Refs
--  > https://www.cs.bham.ac.uk/~vxs/teaching/Haskell/handouts/tail-recursion.pdf
--  > https://stackoverflow.com/questions/4092864/tail-recursion-in-haskell
--  > https://stackoverflow.com/questions/33923/what-is-tail-recursion
--  > https://youtu.be/y6xiaSkVlvs

main :: IO()

-- This is a Guard.
--  We should have boolean expressions
fac n
  | n<=1      = 1
  | otherwise = n * fac(n-1)

-- Pattern matching
--  The _ works as a wild card
is_zero 0 = True
is_zero _ = False

-- Accumulators
--  This implementation here is a tail recursion; the recursive call is the last
--  thing executed. See e.g. https://stackoverflow.com/questions/33923/what-is-tail-recursion
--  Tail recursive algorithms are usually "safer", since it's easier to avoid a stack overflow
--  Here we would have, for fac 4:
--
--      Func n  acc
--      aux  4  1
--      aux  3  (4*1)
--      aux  2  (3*4)
--      aux  1  (2*12)
--      => acc = 24, since n<=1
--
--      Note how this carries out multiplication before calling fac. A näive approach
--      would instead call
--
--      Source Reduction
--      fac 4
--      = 4 * fac (3)
--      = 4 * 3 * 2 * fac (1)
--      = 4 * 3 * 2 * 1
--      = 120
--
--  Thus, note how previous calls in the tail recursion need not to be recorded on
--  the call stack. (So, it's , in a way, equivalent to a iteration)
tail_fac n = aux n 1
  where
    aux n acc
      | n <= 1    = acc
      | otherwise = aux (n-1) (n*acc)

main = do
  putStrLn ("How much is 3! ? It is " ++ (show (fac 3)))
  putStrLn ("Is 2 zero? ? " ++ (show (is_zero 2)))
  putStrLn ("How much is 4! ? It is " ++ (show (tail_fac 4)))
