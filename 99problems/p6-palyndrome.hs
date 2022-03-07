-- The use of the binding ''(Eq a) =>'' here is necessary in order fort the
-- (==) operator to work with the polymorphic type a. We'd get the error:
-- " No instance for (Eq a) arising from a use of ‘==’ ".
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = x == last xs && (isPalindrome $ init $ xs)

-- Another possible solution is:
isPalindromeII :: (Eq a) => [a] -> Bool
isPalindromeII x = x == (reverse x)

-- This is due to the instance (Eq a) for []:
-- *Main> :info []
-- data [] a = [] | a : [a] 	-- Defined in ‘GHC.Types’
-- instance Applicative [] -- Defined in ‘GHC.Base’
-- instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
