{- |
Conceptually, this library provides a way to arbitrarily extend the
global state represented by the IO monad. Viewed another way, this
library provides a basic facility for setting and retrieving values
from global variables. In what some people call 'the real world',
this library simply tosses data into a bunch of files on disk.

The interface takes the form of a very basic key-value store, with
multiple different stores made available through the 'withStore'
function. Stores are referenced by arbitrary strings, and keys
within those stores are treated likewise.

The 'putValue', 'getValue', and 'delValue' functions allow you to
store, retrieve, and delete data from the store. Since the data is
essentially stored as text, it is regrettably necessary to require
that you only store data which is a member of the 'Read' and 'Show'
typeclasses.

Some actions can result in the immediate exit of the entire program,
such as 'executeFile', for example. This will leave a lonely little
directory sitting around in your '/tmp' or equivalent. There is a
roughly 0% chance of ever seeing duplicate data from this, but if it
offends your aesthetics too much, you can go ahead and clean it up
using the 'deleteStore' function from 'System.IO.Storage.Internals'

If you find this library useful, please help lobby for a feature like
this to be added directly to Haskell. Hopefully that would allow us
to dispense with the annoying 'Read' and 'Show' requirements, as well
as (hopefully) allowing typechecking.
-}
module System.IO.Storage
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

import System.IO.Storage.Internals ( getStoragePath, createStore, deleteStore )

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
