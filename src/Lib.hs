module Lib where

import Codec.Archive.Zip
import Control.Monad.IO.Class
import Control.Monad.Catch
import Path
import Path.Internal

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Creates an empty jar archive at the given relative filepath location.
-- The name of the archive and its file ending should be included in the
-- filepath.
--
-- For example, passing in "src/Main.jar" would create an empty jar archive
-- named "Main.jar" in the "src" sub-directory of the current directory.
--
-- Note that as the function returns a Maybe action, the inner action must be
-- evaluated in order for the jar to be created. This can be done by seperating
-- the action from the Maybe like in the example below:
--
-- @
-- case createJar "src/Main.jar" of
--   Just action -> action
-- @
createEmptyJar :: (MonadIO m, MonadCatch m) => FilePath -> Maybe (m ())
createEmptyJar location = maybeZipAction
  where path = parseRelFile location
        maybeZipAction = case path of
                           Just p  -> Just (createArchive p (return()))
                           Nothing -> Nothing

-- | Converts a String representation of a the relative path of a file into the
-- corresponding relative file path.
fileNameToPath :: FilePath -> Path Rel File
fileNameToPath file = path
  where path = case parseRelFile file of
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

                       {-
                        λ ~ import Codec.Archive.Zip
λ ~ let f1 = case f of;  Just p -> p
λ ~ f1
"src/Test1.class"
λ ~ :t f
f :: Control.Monad.Catch.MonadThrow m => m (Path Rel File)
λ ~ :t f1
f1 :: Path Rel File
λ ~ let add1 = addEntry Store (packChars "test") f1

<interactive>:23:46:
    Couldn't match expected type ‘EntrySelector’
                with actual type ‘Path Rel File’
    In the third argument of ‘addEntry’, namely ‘f1’
    In the expression: addEntry Store (packChars "test") f1
λ ~ let add1 = addEntry Store (packChars "test") (mkEntrySelector f1)

<interactive>:24:47:
    Couldn't match expected type ‘EntrySelector’
                with actual type ‘m0 EntrySelector’
    In the third argument of ‘addEntry’, namely ‘(mkEntrySelector f1)’
    In the expression:
      addEntry Store (packChars "test") (mkEntrySelector f1)
λ ~ let entrySel = case mkEntrySelector f1 of;  Just e -> e
λ ~ :t entrySel 
entrySel :: EntrySelector
λ ~ let add1 = addEntry Store (packChars "test") (entrySel)
λ ~ let j = parseRelFile "src/Test.jar"
λ ~ let j1 = case j of;  Just p -> p
λ ~ withArchive j1 add1
*** Exception: src/Test.jar: canonicalizePath: does not exist (No such file or directory)
λ ~ createJar "src/Test.jar"
λ ~ withArchive j1 add1
λ ~ 
-}
