module Lib where

import Codec.Archive.Zip
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.ByteString.Internal
import Path
import Path.Internal

-- | Creates an empty jar archive at the given relative filepath location.
-- The name of the archive and its file ending should be included in the
-- filepath.
--
-- For example, passing in "src\/Main.jar" would create an empty jar archive
-- named "Main.jar" in the "src" sub-directory of the current directory.
createEmptyJar :: (MonadIO m, MonadCatch m) => FilePath -> m ()
createEmptyJar location = maybeZipAction
  where path = parseRelFile location
        maybeZipAction = do p <- path
                            createArchive p (return ())

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
addByteStringToJar :: (MonadThrow m, MonadIO m) => FilePath -> ByteString -> FilePath -> m ()
addByteStringToJar fileLocation contents jarLocation = maybeZip
  where maybeZip = jarPath >>= ((flip withArchive) zipAction)
        jarPath = parseRelFile jarLocation
        zipAction = entrySel >>= (addEntry Store contents)
        entrySel = filePath >>= mkEntrySelector
        filePath = parseRelFile fileLocation
