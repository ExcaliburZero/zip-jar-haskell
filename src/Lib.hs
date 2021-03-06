module Lib where

import Codec.Archive.Zip (addEntry, CompressionMethod(Store), createArchive, mkEntrySelector, withArchive)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.ByteString.Internal (ByteString)
import Path (parseRelFile)

-- | Creates an empty jar archive at the given relative filepath location.
-- The name of the archive and its file ending should be included in the
-- filepath.
--
-- __Throws__: 'PathParseException'
--
-- See 'createArchive' and 'parseRelFile' for more information.
--
-- For example, passing in "src\/Main.jar" would create an empty jar archive
-- named "Main.jar" in the "src" sub-directory of the current directory.
--
-- @
-- createEmptyJar "src/Main.jar"
-- @
--
-- __Before__
--
-- @
-- .
-- └── src
-- @
--
-- __After__
--
-- @
-- .
-- └── src
--     └── Main.jar
-- @
createEmptyJar :: (MonadIO m, MonadCatch m) => FilePath -> m ()
createEmptyJar location = zipAction
  where path = parseRelFile location
        zipAction = do p <- path
                       createArchive p (return ())

-- | Adds the given ByteString as a file at the given location within the given
-- jar archive.
--
-- __Throws__: 'PathParseException', 'EntrySelectorException',
-- isAlreadyInUseError, isDoesNotExistError, isPermissionError, 'ParsingFailed'
--
-- See 'withArchive', 'mkEntrySelector', and 'parseRelFile' for more information.
--
-- For example, running the following would create a file named "Hello.class"
-- containing the string "Hello, World!" within the "src" directory in the jar
-- archive located at "build\/libs\/HelloWorld.jar".
--
-- @
-- let fileLocation = "src\/Hello.class"
-- let contents = packChars "Hello, World!"
-- let jarLocation = "build\/libs\/HelloWorld.jar"
-- addByteStringToJar fileLocation contents jarLocation
-- @
--
-- __Before__
--
-- @
-- .
-- └── build
--     └── libs
--         └── HelloWorld.jar
-- @
--
-- __After__
--
-- @
-- .
-- └── build
--     └── libs
--         └── HelloWorld.jar
--             └── src
--                 └── Hello.class
-- @
addByteStringToJar :: (MonadThrow m, MonadIO m)
  => FilePath      -- ^ Location of the new file within the jar
  -> ByteString    -- ^ Contents of the new file to add
  -> FilePath      -- ^ Location of the jar to add the new file into
  -> m ()
addByteStringToJar fileLocation contents jarLocation = zipAction
  where zipAction = jarPath >>= flip withArchive zipChange
        zipChange = entrySel >>= addEntry Store contents
        entrySel  = filePath >>= mkEntrySelector
        jarPath   = parseRelFile jarLocation
        filePath  = parseRelFile fileLocation
