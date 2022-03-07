data NestedList a = Elem a | List [NestedList a] deriving Show

flatten :: NestedList a -> [a]
flatten (List [])     = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- *Main> flatten (List [Elem 5, List [Elem 44, List [Elem 30, Elem 11], Elem 23]])
-- [5,44,30,11,23]
-- *Main> flatten (Elem 5)
-- [5]
-- *Main>
