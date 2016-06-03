module Lib where

import Codec.Archive.Zip
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.ByteString.Internal
import Path
import Path.Internal

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Creates an empty jar archive at the given relative filepath location.
-- The name of the archive and its file ending should be included in the
-- filepath.
--
-- For example, passing in "src\/Main.jar" would create an empty jar archive
-- named "Main.jar" in the "src" sub-directory of the current directory.
createEmptyJar :: (MonadIO m, MonadCatch m) => FilePath -> Maybe (m ())
createEmptyJar location = maybeZipAction
  where path = parseRelFile location
        maybeZipAction = do p <- path
                            createArchive p (return ())

-- | Converts a String representation of a the relative path of a file into the
-- corresponding relative file path.
fileNameToPath :: FilePath -> Path Rel File
fileNameToPath file = path
  where path = case parseRelFile file of
                 Just p -> p

-- | Adds the given ByteString as a file at the given location within the given
-- jar archive.
--
-- For example, running the following would create a file named "Hello.class"
-- containing the string "Hello, World!" within the "src" directory in the jar
-- archive located at "build\/libs\/HelloWorld.jar".
--
-- @
-- let contents = packChars "Hello, World!"
-- addByteStringToJar "src\/Hello.class" contents "build\/libs\/HelloWorld.jar"
-- @
--addByteStringToJar :: (MonadIO m, MonadCatch m) => FilePath -> ByteString -> FilePath -> m ()
{-
addByteStringToJar fileLocation contents jarLocation = maybeZip
  where maybeZip  = case jarPath of
                      Just j  -> case zipAction of
                                   Just a  -> Just (withArchive j a)
                                   Nothing -> Nothing
                      Nothing -> Nothing
  --where maybeZip  = withArchive jarPath zipAction
        jarPath   = parseRelFile jarLocation
        zipAction = case entrySel of
                      Just e  -> Just (addEntry Store contents e)
                      Nothing -> Nothing
        --zipAction = addEntry Store contents entrySel
        entrySel  = case filePath of
                      Just p  -> Just (mkEntrySelector p)
                      Nothing -> Nothing
        --entrySel  = case mkEntrySelector filePath of
        --              Just e -> e
        filePath  = parseRelFile fileLocation
-}

{-
λ ~ import Data.ByteString.Internal
λ ~ let contents = packChars "Testing"
λ ~ addByteStringToJar "src/Main.txt" contents "src/Maybe.jar"
-}


--pathsToJar files = do paths <- map parseRelFile files
--                      classesToJar paths

--classesToJar :: [ClassFile] -> JarName -> JarArchive
--classesToJar :: [Path Rel File] -> IO ()
--classesToJar filePaths = jarArchive
--  where jarArchive = map mkEntrySelector filePaths
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
