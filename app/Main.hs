module Main
  ( main
  ) where

import           Lib
import           System.Environment             ( getArgs )

main :: IO ()
-- main = mapM_ (bruteForce "...") [1 .. 10]
main = do
  args <- getArgs
  file <- readFile (head args)
  let termified   = fromBirbs file
  let rebirbified = fromTerm termified
  putStrLn $ "input: " ++ rebirbified
  normalBirbs <- nf termified
  let retermified = fromTerm normalBirbs
  putStrLn $ "reduced: " ++ retermified
  return ()
