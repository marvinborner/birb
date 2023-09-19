module Main
  ( main
  ) where

import           Lib
import           System.Environment             ( getArgs )
import           Term
import           Transpile

transpile :: String -> IO ()
transpile path = do
  file <- readFile path
  let term = fromBLC file
  let ski  = transpileSKI term
  let birb = transpileBirb ski
  putStrLn $ concatMap show birb

reduce :: String -> IO ()
reduce path = do
  file <- readFile path
  let termified   = fromBirbs file
  let rebirbified = fromTerm termified
  putStrLn $ "input: " ++ rebirbified
  normalBirbs <- nf termified
  let retermified = fromTerm normalBirbs
  putStrLn $ "reduced: " ++ retermified

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["transpile", path] -> transpile path
    ["reduce"   , path] -> reduce path
    _                   -> putStrLn "Usage: birb [transpile|reduce] <file>"
