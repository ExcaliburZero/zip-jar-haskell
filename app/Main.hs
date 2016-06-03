module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
  args <- getArgs
  let jarLocation = head args
  createEmptyJar jarLocation
  return ()
