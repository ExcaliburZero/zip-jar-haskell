module Lib where

import Codec.Archive.Zip
import Path

someFunc :: IO ()
someFunc = putStrLn "someFunc"

pathsToJar files = do paths <- map parseRelFile files
                      classesToJar paths
                      

--classesToJar :: [ClassFile] -> JarName -> JarArchive
--classesToJar :: [Path Rel File] -> IO ()
classesToJar filePaths = jarArchive
  where jarArchive = map mkEntrySelector filePaths
--        filePaths  = map parseRelFile fileNames
