module Main where

import Codec.Archive.Zip
import System.Environment (getArgs)

import Lib

--main :: IO ()
main = do
  args <- getArgs
  let jarLocation = head args
  case createEmptyJar jarLocation of
    Just x -> x
{-
  let x = case createEmptyJar jarLocation of
            Just action -> action
  return x
-}
