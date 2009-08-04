{- |

Conceptually, this library embeds a very basic key-value store
in the IO monad. Realistically, it does this by means of a
temporary directory holding files.

The API is almost too simple to bother mentioning. All operations
take a string 'Store Name', which is a namespace for keys. Keys are
always strings, and values must be members of the 'Read' and 'Show'
yypeclasses. Supported operations include storing data, getting data,
deleting keys, and destroying an entire store.

One thing to note is that the 'getValue' operation is designed to return
'IO Nothing' rather than ever allow an exception to escape. That means
that a read error will manifest as your application recieving 'Nothing'.

It is recommended that programs take care to call 'clearAll' before or
after using a store, as a precaution against the (very unlikely) situation
where a program manages to access ghost data from a previous invocation.

-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.IO.Storage
    ( putValue
    , getValue
    , delValue
    , clearAll
    , getValueDefault
    ) where

import System.IO            ( readFile, writeFile )
import System.IO.Error      ( try )
import System.FilePath      ( (</>) )
import System.Directory     ( getTemporaryDirectory, createDirectoryIfMissing
                            , doesFileExist, getDirectoryContents, removeFile
                            , removeDirectory, removeDirectoryRecursive )
import System.Environment   ( getProgName )
import Data.List            ( (\\) )
import Data.Maybe           ( fromMaybe )

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

import System.Win32 ( DWORD )
-- This can be removed as soon as a 'getProcessID' function
-- gets added to 'System.Win32'
foreign import stdcall unsafe "winbase.h GetCurrentProcessId"
    c_GetCurrentProcessID :: IO DWORD

getPIDString = fmap show c_GetCurrentProcessID
specialChars = "<>"

#else

import System.Posix.Process ( getProcessID )
getPIDString = fmap show getProcessID
specialChars = ""

#endif

getIdentifier = do
    progName <- getProgName
    procID   <- getPIDString
    let idName = "kv-store-" ++ progName ++ "-" ++ procID
    return . stripSpecials $ idName
  where stripSpecials = filter $ not . (`elem` specialChars)

-- | We generate the storage path from the program name combined
--   with the PID. We basically just have to hope we don't get
--   the same *both* before the temp dir gets cleared.
getStoragePath :: String -> IO String
getStoragePath db = do
    identifier <- getIdentifier
    tempPath <- getTemporaryDirectory
    return $ tempPath </> identifier </> db

-- | Stores a value
putValue :: Show a => String -> String -> a -> IO ()
putValue db key value = do
    kvDir <- getStoragePath db
    createDirectoryIfMissing True kvDir
    let fileName = kvDir </> key
    writeFile fileName $ show value

-- | Gets a value. Will return Nothing if anything goes wrong.
getValue :: Read a => String -> String -> IO (Maybe a)
getValue db key = do
    kvDir <- getStoragePath db
    let fileName = kvDir </> key
    keyExists <- doesFileExist fileName
    if keyExists
       then do fileData <- readFile fileName
               tryData <- try $ readIO fileData
               case tryData of
                    Left  _ -> return $ Nothing
                    Right v -> return $ Just v
       else return Nothing

getValueDefault :: Read a => a -> String -> String -> IO a
getValueDefault v db key = fmap (fromMaybe v) (getValue db key)

-- | Delete a value from the store.
delValue :: String -> String -> IO ()
delValue db key = do
    kvDir <- getStoragePath db
    let fileName = kvDir </> key
    tryData <- try $ removeFile key
    case tryData of
         Left  _ -> return ()
         Right _ -> return ()

-- | Clear an entire store. Try to call this before or after
--   using a store, or both if possible.
clearAll :: String -> IO ()
clearAll db = do
    kvDir <- getStoragePath db
    createDirectoryIfMissing True kvDir
    removeDirectoryRecursive kvDir

    -- Remove the program storage if necessary
    progStorage <- getStoragePath ""
    contents <- getDirectoryContents progStorage
    if null $ contents \\ [".", ".."]
       then removeDirectory progStorage
       else return ()
