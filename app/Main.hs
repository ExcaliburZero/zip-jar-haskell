module Main where

import Data.ByteString.Internal (packChars)
import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
  args <- getArgs
  let jarLocation = head args
  createEmptyJar jarLocation
  let fileLocation = args !! 1
  let fileContents = packChars $ args !! 2
  addByteStringToJar fileLocation fileContents jarLocation
  return ()
