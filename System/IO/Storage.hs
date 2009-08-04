{- |
Conceptually, this library provides a way to arbitrarily extend the
global state represented by the IO monad. Viewed another way, this
library provides a basic facility for setting and retrieving values
from global variables.

The interface takes the form of a very basic key-value store, with
multiple different stores made available through the 'withStore'
function. Stores are referenced by arbitrary strings, and keys
within those stores are treated likewise. The 'putValue', 'getValue',
and 'delValue' functions allow you to store, retrieve, and delete
data from the store.

Internally, data is stored within an IORef which is created using the
'unsafePerformIO hack', but this is not made available outside of the
library so that it can easily be modified if and when a more 'proper'
solution is implemented.
-}
module System.IO.Storage
  ( withStore
  , getValue
  , putValue
  , delValue
  , getDefaultValue
  ) where

import Data.IORef        ( IORef, newIORef, modifyIORef, readIORef )
import Data.List as L    ( lookup, deleteFirstsBy )
import Data.Map as M     ( Map, empty, lookup, insert, delete )
import Data.Dynamic      ( Dynamic, toDyn, fromDyn, fromDynamic )
import Data.Typeable     ( Typeable )
import Data.Function     ( on )
import Control.Exception ( bracket )
import System.IO.Unsafe  ( unsafePerformIO )

type ValueStore = M.Map String Dynamic

globalPeg :: IORef [(String, IORef ValueStore)]
{-# NOINLINE globalPeg #-}
globalPeg = unsafePerformIO (newIORef [])

withStore :: String -> IO a -> IO a
withStore storeName action = do
    store <- newIORef M.empty
    let emptyStore = (storeName, store)
    let create = modifyIORef globalPeg (emptyStore:)
    let delete = modifyIORef globalPeg deleteStore
    bracket create (const delete) (const action)
  where deleteStore xs = deleteFirstsBy ((==) `on` fst) xs dummyStore
        dummyStore = [(storeName, undefined)]

getPrimitive :: String -> String -> IO (Maybe Dynamic)
getPrimitive storeName key = do
    storeList <- readIORef globalPeg
    case storeName `L.lookup` storeList of
         Nothing -> return Nothing
         Just st -> do map <- readIORef st
                       return $ key `M.lookup` map

getValue :: Typeable a => String -> String -> IO (Maybe a)
getValue storeName key = do
    value <- getPrimitive storeName key
    case value of
         Nothing -> return $ Nothing
         Just dy -> return $ fromDynamic dy

getDefaultValue :: Typeable a => String -> String -> a -> IO a
getDefaultValue storeName key val = do
    value <- getPrimitive storeName key
    case value of
         Nothing -> return $ val
         Just dy -> return $ fromDyn dy val

putValue :: Typeable a => String -> String -> a -> IO ()
putValue storeName key value = do
    storeList <- readIORef globalPeg
    case storeName `L.lookup` storeList of
         Nothing -> return ()
         Just st -> modifyIORef st . M.insert key . toDyn $ value

delValue :: String -> String -> IO ()
delValue storeName key = do
    storeList <- readIORef globalPeg
    case storeName `L.lookup` storeList of
         Nothing -> return ()
         Just st -> modifyIORef st . M.delete $ key
