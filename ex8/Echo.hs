-- Compile with: ghc --make Echo.hs

module Main
where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn (unwords args)
