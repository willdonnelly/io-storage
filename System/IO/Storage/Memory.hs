module System.IO.Storage.Memory
  ( withStore
  , getValue
  , putValue
  , delValue
  ) where

import Data.IORef        ( IORef, newIORef, modifyIORef, readIORef )
import Data.List as L    ( lookup, deleteFirstsBy )
import Data.Map as M     ( Map, empty, lookup, insert, delete )
import Data.Dynamic      ( Dynamic, toDyn, fromDynamic )
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

getValue storeName key = do
    storeList <- readIORef globalPeg
    case storeName `L.lookup` storeList of
         Nothing -> return $ Nothing
         Just st -> do map <- readIORef st
                       case key `M.lookup` map of
                            Nothing -> return $ Nothing
                            Just dy -> return $ fromDynamic dy

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
