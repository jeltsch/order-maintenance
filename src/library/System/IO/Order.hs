module System.IO.Order (

    newMinimum,
    newMaximum,
    newAfter,
    newBefore

) where

-- Data

import Data.Order
import Data.Order.Internal as Internal

-- System

import System.IO.Unsafe

newMinimum :: IO (Element Global)
newMinimum = Internals.newMinimum

newMaximum :: IO (Element Global)
newMaximum = Internals.newMaximum

newAfter :: Element Global -> IO (Element Global)
newAfter elem = Internals.newAfter elem

newBefore :: Element Global -> IO (Element Global)
newBefore elem = Internals.newBefore elem

{-# NOINLINE globalOrderRep #-}
globalOrderRep :: OrderRep Global
globalOrderRep = unsafePerformIO $ newOrderRep defaultRawAlg
