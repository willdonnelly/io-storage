module System.IO.Storage.FileSystem
  ( withStore
  , putValue
  , getValue
  , delValue
  ) where

import System.IO            ( readFile, writeFile )
import System.IO.Error      ( try )
import System.Directory     ( doesFileExist, removeFile )
import System.FilePath      ( (</>) )
import Control.Exception    ( bracket )

import System.IO.Storage.FileSystem.Internals
           ( getStoragePath, createStore, deleteStore )

-- | Initialize a key-value store with the given name.
--   attempting to use any of the other functions on a
--   store that hasn't been initialized will probably
--   cause everything to explode horribly.
withStore :: String -> IO a -> IO a
withStore storeName action = bracket create delete $ const action
  where create = createStore storeName
        delete = const $ deleteStore storeName

-- | Stores a value. Explodes if the store hasn't been
--   initialized.
putValue :: Show a => String -> String -> a -> IO ()
putValue store key value = do
    storePath <- getStoragePath store
    writeFile (storePath </> key) $ show value

-- | Gets a value. Will return Nothing if the key isn't
--   there.
getValue :: Read a => String -> String -> IO (Maybe a)
getValue store key = do
    storePath <- getStoragePath store
    let fileName = storePath </> key
    keyExists <- doesFileExist fileName
    if keyExists
       then readFile fileName >>= return . Just . read
       else return Nothing

-- | Delete a value from the store.
delValue :: String -> String -> IO ()
delValue store key = do
    storePath <- getStoragePath store
    try . removeFile $ storePath </> key
    return ()
