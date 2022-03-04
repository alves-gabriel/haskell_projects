-- # Commands & Topics
-- Here we learn
--  > Monads
--  > Bind
--  > The (>>) operator
--
--
-- # Hints:
--
-- # Refs
--  > https://www.youtube.com/watch?v=IBB7JpbClo8
--  > https://stackoverflow.com/questions/28139259/why-do-we-need-monads#:~:text=Monads%20are%20just%20a%20convenient,an%20element%20type%20(%20return%20).
--
--  Copy template with: cat template.hs >> "New file"
--

main :: IO()

--
-- Prelude> :info Monad
-- class Applicative m => Monad (m :: * -> *) where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
--   fail :: String -> m a
--   {-# MINIMAL (>>=) #-}
--   	-- Defined in ‘GHC.Base’
-- instance Monad (Either e) -- Defined in ‘Data.Either’
-- instance Monad [] -- Defined in ‘GHC.Base’
-- instance Monad Maybe -- Defined in ‘GHC.Base’
-- instance Monad IO -- Defined in ‘GHC.Base’
-- instance Monad ((->) r) -- Defined in ‘GHC.Base’
-- instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’
--
--  Let's take a look at three functions. First at the bind function:
--
--  (>>=) :: m a -> (a -> m b) -> m b
--
--  We can see its effect:
--
--  Prelude> Just 1 >>= (\x -> Just x)
--  Just 1
--  Prelude> Nothing >>= (\x -> Just x)
--  Nothing
--
--  This bind operator. We get a monad of type a, then a function which takes
--  a to a monad of type b, and we get a monad of type b. So, what we get is
--  the internal type of the monad. It extracts that value.
--
--  How can we put this in practice? Let's define a function called maybeadd
--  which takes an y and a maybe of x and adds them together.
maybeadd :: Num b => Maybe b -> b -> Maybe b
maybeadd mx y = mx >>= (\x -> Just $ x + y)

--  *Main> maybeadd (Just  5) 44
--  Just 49
--  *Main> maybeadd Nothing 30
--  Nothing
--  *Main>

--  We also havethe return function, which returns the monad! We can implement.
--  Let's experiment with a function which has a maybe for both x and y. Note how
--  this also changes the types! This is a function of monads of b in both arguments,
--  which also outputs a monad of b! We can use this in any monad which has an internal
--  type which has this Num typle class. E.g., the IO(), etc...
--
-- *Main> monadd (Just 11) (Just 23)
-- Just 34
monadd :: (Monad m, Num b) => m b -> m b -> m b
monadd mx my = mx >>= (\x -> my>>= (\y -> return $ x+y))

--  This syntax m >>= (\x -> ...) is kinda convoluted, an alternative is the do notation:
--
--  do
--    x <- main
--    ...
--
--  What this notation is saying is: take the internal value of m, put it into x
--  and then do something else. However, note that if there is a faulty state in the monad
--  e.g. a nothing, we PROPAGATE this error through. We don't have to think about them in this case!
--  We can create pure functions which can still errors and exceptions which happens on the side!
monadd_simple mx my = do
  x <- mx
  y <- my
  return $ x+y

--  What is the actual implementation of Maybe? (This notation below is also a way of doing pattern matching)
--
-- instance Monad Maybe where
--   m >>= f = case m of
--               Nothing -> Nothing
--               Just x -> f x
--   return v = Just v
--
--  Finally, the (>>) operator.
--
--  (>>) :: Monad m >= m a -> m b -> m b
--  m >> n = m >>= \_ -> n
--
--  (This \_ -> n on the RHS is just an anonymous function)
--
--  In this operator we just ignore the value we get and continue with whatever we wanted to do
--  The important thing is that if we got a faulty state in m, it is propragated through. See:
--
--  *Main> Nothing >> Just 34
--   Nothing
--  *Main> Just 5 >> Just 44
--  Just 44
--  *Main> Just 30 >> Nothing
--  Nothing
--
--  Act:
--
--  Sometimes we don't care about what the output is, but we want still to perform error check (e.g. IO actions)
--  and still propragate the error through.
--
--  act >> ...
--  do
--   act
--   ...
--
--  The expression above shows that act together with the (>>) operator is the
--  same as the do notation with no regard witht the value we get back.

main =
  do
    putStrLn (" " ++ (show ( )))
