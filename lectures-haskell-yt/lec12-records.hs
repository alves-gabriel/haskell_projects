-- # Commands & Topics
-- Here we learn
--  > Records
--
-- # Hints:
--
-- # Refs
--  > https://www.youtube.com/watch?v=CoNZRKqQi1o
--  > http://learnyouahaskell.com/making-our-own-types-and-typeclasses
--
--  Copy template with: cat template.hs >> "New file"

main :: IO()

-- We use records in order to construct with our own typeclasses!
--
-- Ex: functions dealing with "Person" should handle name and age, e.g.
-- data Person = Person String Int
-- This is not so nice because the data type does not reflect that. We can use records:
data Person = PersonData { name :: String, age :: Int }

-- From this definition, automatically these functions are generated:
--
--  Prelude> :type name
--  name :: Person -> String
--  Prelude> :type age
--  age :: Person -> Int
--
-- Here "data" means we're defining a new data type called Person.
-- Before "=": the data type itself
-- After "=": what we call value constructors (or data constructiors), i.e.
-- the different values this type can have. E.g, if we were to define bool like
-- this, we would make:
--
-- data Bool = False | True
--
-- Here "|" reads as or. For Int we'd have:
--
-- data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
--
-- If we were to run this on ghci we would get:
--
-- *Main> :t PersonData
-- PersonData :: String -> Int -> Person
--
-- So, we can for example write a function like this:
greet :: Person -> [Char]
greet person = "Hi " ++ name person

-- If we do want to manually work with the type, we could do something like:
greet2 :: Person -> [Char]
greet2 (PersonData name _) = "Hi " ++ name

-- What we call the variables is also not important, only their order. I could've
-- written:
--greet2 (Person n _) = "Hi " ++ n

-- To finish, note that records can have multiple constructors. And the functions
-- that are automatically generated work on various constructors.
-- Below, D2 and D3 are two and three dimensional points. By calling x, y or z
-- as functions we can extract their values. Ex:
-- z (D3 1 2 3) would work
-- but not
-- z (D2 1 2)
data Point =
    D2 { x :: Int, y :: Int}
  | D3 { x :: Int, y :: Int, z :: Int}

-- Running this on GHCI we get:
-- *Main> :t D3
-- D3 :: Int -> Int -> Int -> Point

main =
  do
    putStrLn ("The point x of (1 2) is: " ++ (show $ x (D2 1 2)))
    putStrLn ("The point z of (1 2 3) is: " ++ (show $ z (D3 1 2 3)))

    -- Using the record syntax from the beginning
    putStrLn ("Hi " ++ (show $ name $ PersonData "Sebastian" 5))

    let ham = PersonData "Lewis" 44
    putStrLn (show $ greet ham)
