module Main
  ( main
  ) where

import           Lib
import           System.Environment             ( getArgs )
import           Term
import           Transpile

transpile :: IO ()
transpile = do
  args <- getArgs
  file <- readFile (head args)
  let term = fromBLC file
  putStrLn $ "input: " ++ show term
  let ski = transpileSKI term
  putStrLn $ "SKI transpiled: " ++ show ski
  let birb = transpileBirb ski
  putStrLn $ "Birb transpiled: " ++ show birb
  return ()

reduce :: IO ()
reduce = do
  args <- getArgs
  file <- readFile (head args)
  let termified   = fromBirbs file
  let rebirbified = fromTerm termified
  putStrLn $ "input: " ++ rebirbified
  normalBirbs <- nf termified
  let retermified = fromTerm normalBirbs
  putStrLn $ "reduced: " ++ retermified
  return ()

main :: IO ()
main = transpile
-- main = mapM_ (bruteForce "...") [1 .. 10]
