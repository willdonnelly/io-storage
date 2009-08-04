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

This library actually implements its functionality in two different
ways. 'System.IO.Storage.FileSystem' simply stores its files in a
temporary directory in the filesystem. This means that it requires
all values to be 'Read' and 'Show'-able. On the other hand, the
'System.IO.Storage.Memory' implementation uses the 'unsafePerformIO'
hack to store everything completely in memory. It, unfortunately,
requires that data be 'Typeable'.

Simply importing 'System.IO.Storage' is equivalent to using the
memory backend, but allows the library to be modified behind the
scenes. It is recommended that you simply use this, and not worry
about the implementation.
-}
module System.IO.Storage
  ( withStore
  , getValue
  , putValue
  , delValue
  , getDefaultValue
  ) where

import System.IO.Storage.Memory
