module Lib where

import Codec.Archive.Zip
import Path
import Path.Internal

someFunc :: IO ()
someFunc = putStrLn "someFunc"

pathsToJar files = do paths <- map parseRelFile files
                      classesToJar paths

--classesToJar :: [ClassFile] -> JarName -> JarArchive
--classesToJar :: [Path Rel File] -> IO ()
classesToJar filePaths = jarArchive
  where jarArchive = map mkEntrySelector filePaths
--        filePaths  = map parseRelFile fileNames
--

createJar pathE = createArchive path (return())
  where path = case pathE of
                 Just p -> p
