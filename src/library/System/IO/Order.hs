module System.IO.Order (

    newMinimum,
    newMaximum,
    newAfter,
    newBefore

) where

-- Data

import           Data.Order
import           Data.Order.Internals (OrderRep, newOrderRep)
import qualified Data.Order.Internals as Internals
import           Data.Order.Raw.Algorithm

-- System

import System.IO.Unsafe

newMinimum :: IO (Element Global)
newMinimum = Internals.newMinimum globalOrderRep

newMaximum :: IO (Element Global)
newMaximum = Internals.newMaximum globalOrderRep

newAfter :: Element Global -> IO (Element Global)
newAfter elem = Internals.newAfter elem globalOrderRep

newBefore :: Element Global -> IO (Element Global)
newBefore elem = Internals.newBefore elem globalOrderRep

{-# NOINLINE globalOrderRep #-}
globalOrderRep :: OrderRep Global
globalOrderRep = unsafePerformIO $ newOrderRep defaultRawAlgorithm
