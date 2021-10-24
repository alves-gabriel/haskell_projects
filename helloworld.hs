-- # Tools
--  Atom package: apm install language-haskell ide-haskell ide-haskell-cabal, see

-- # Execution: runhaskell helloworld.hs
--  or ghci  helloworld.hs (compiled)
-- (or ghc to make build files. The i stands for interactive)
--  Quit with Ctrl + D

-- # Refs
--  > https://riptutorial.com/haskell/example/898/hello--world-
--  > https://atom-haskell.github.io/installation/installing-packages/ (Atom
--  package: apm install language-haskell ide-haskell ide-haskell-cabal)
-- > http://learnyouahaskell.com/starting-out

-- # Hint: :reload or :r reloads everything in the GHCI

-- Type annotation, main is type IO. It's optional (but recommended)
main :: IO ()
main = putStrLn "Hello, World!"
