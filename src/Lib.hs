module Lib where

import Codec.Archive.Zip
import Control.Monad.IO.Class
import Control.Monad.Catch
import Path
import Path.Internal

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Creates an empty jar archive at the given relative filepath location.
-- The name of the archive and itsfile ending should be included in the
-- filepath.
--
-- For example, passing in "src/Main.jar" would create an empty jar archive
-- named "Main.jar" in the "src" sub-directory of the current directory
createJar :: (MonadIO m, MonadCatch m) => FilePath -> m ()
createJar location = createArchive path (return())
  where path = case parseRelFile location of
                 Just p -> p

pathsToJar files = do paths <- map parseRelFile files
                      classesToJar paths

--classesToJar :: [ClassFile] -> JarName -> JarArchive
--classesToJar :: [Path Rel File] -> IO ()
classesToJar filePaths = jarArchive
  where jarArchive = map mkEntrySelector filePaths
--        filePaths  = map parseRelFile fileNames
--


{-
addToJar jar file = do zipActionNoM <- zipAction
                       withArchive jarPath zipAction
                       return ()
  where jarPath  = case parseRelFile jar of
                     Just p -> p
        filePath = case parseRelFile file of
                     Just p -> p
        fileContents = readFile file
        zipAction = do fileContentsNoM <- fileContents
                       addEntry Store fileContentsNoM (mkEntrySelector filePath)
                       -}
