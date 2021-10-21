-- # Refs
--  > https://riptutorial.com/haskell/example/898/hello--world-
--  > https://atom-haskell.github.io/installation/installing-packages/ (Atom
--  package: apm install language-haskell ide-haskell ide-haskell-cabal)
-- > http://learnyouahaskell.com/starting-out

-- # Execution: runhaskell helloworld.hs
--  or ghc helloworld.hs (compiled)
--  Atom package: apm install language-haskell ide-haskell ide-haskell-cabal, see

-- # Hint: :reload or :r reloads everything in the GHCI

-- Type annotation, main is type IO. It's optional (but recommended)
main :: IO ()
main = putStrLn "Hello, World!"
