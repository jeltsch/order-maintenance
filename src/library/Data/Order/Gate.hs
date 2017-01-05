module Data.Order.Gate (

    Gate,
    newGate,
    withRawOrder

) where

-- Control

import Control.Monad.ST
import Control.Exception
import Control.Concurrent.MVar

-- Data

import Data.Order.Algorithm.Raw

newtype Gate o = Gate (MVar (RawOrder RealWorld o))

newGate :: RawOrder RealWorld o -> IO (Gate o)
newGate rawOrder = Gate <$> newMVar rawOrder

withRawOrder :: Gate o -> (RawOrder RealWorld o -> IO r) -> IO r
withRawOrder (Gate mVar) cont = bracket (takeMVar mVar) (putMVar mVar) cont
