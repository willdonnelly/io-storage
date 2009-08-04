module System.IO.Storage.Memory
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

getValue storeName key = do
    value <- getPrimitive storeName key
    return . fmap fromDynamic $ value

getDefaultValue storeName key val = do
    value <- getPrimitive storeName key
    return . fmap (fromDyn val) $ value

putValue storeName key value = do
    storeList <- readIORef globalPeg
    case storeName `L.lookup` storeList of
         Nothing -> return ()
         Just st -> modifyIORef st . M.insert key . toDyn $ value

delValue storeName key = do
    storeList <- readIORef globalPeg
    case storeName `L.lookup` storeList of
         Nothing -> return ()
         Just st -> modifyIORef st . M.delete $ key
