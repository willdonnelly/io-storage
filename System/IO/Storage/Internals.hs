{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module System.IO.Storage.Internals where

import System.Environment   ( getProgName )
import System.FilePath      ( (</>) )
import System.Directory     ( getTemporaryDirectory, createDirectoryIfMissing
                            , getDirectoryContents, removeDirectoryRecursive
                            , removeDirectory )
import Data.List            ( (\\) )

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

-- | We generate the storage path from the program name combined
--   with the PID. We basically just have to hope we don't get
--   the same *both* before the temp dir gets cleared, or else
--   we hope that the storage path was cleared properly instead.
getStoragePath :: String -> IO FilePath
getStoragePath storeName = do
    tempPath <- getTemporaryDirectory
    progName <- getProgName
    procID   <- getPIDString
    let storePath = "kv-store-" ++ progName ++ "-" ++ procID
    return . stripSpecials $ tempPath </> storePath </> storeName
  where stripSpecials = filter $ not . (`elem` specialChars)

-- | Creates a single data-store. Guarantees that stores will be entirely
--   empty when created. Should not be used directly under any circumstances,
--   as 'withStore' is to be greatly preferred.
createStore :: String -> IO ()
createStore storeName = do
    storeDir <- getStoragePath storeName
    createDirectoryIfMissing True storeDir
    removeDirectoryRecursive storeDir
    createDirectoryIfMissing True storeDir

-- | Deletes a single data-store. Under ordinary circumstances, this
--   should never be used, and the program should rely upon 'withStore'
--   to clean up properly. This needs to be used when control is known
--   to leave the program entirely, such as when using 'execFile'. Even
--   then, it is merely an aesthetic consideration, as the store will
--   be cleared anyway when 'withStore' is called.
deleteStore :: String -> IO ()
deleteStore storeName = do
    -- Remove the current store
    storeDir <- getStoragePath storeName
    createDirectoryIfMissing True storeDir
    removeDirectoryRecursive storeDir
    -- Remove the program store path if necessary
    storageDir <- getStoragePath ""
    dirContent <- getDirectoryContents storageDir
    if null $ dirContent \\ [".", ".."]
       then removeDirectory storageDir
       else return ()
