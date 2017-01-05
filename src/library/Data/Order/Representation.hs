module Data.Order.Representation (

    OrderRep (OrderRep),
    newOrderRep

) where

-- Control

import Control.Monad.ST

-- Data

import Data.Order.Algorithm.Raw
import Data.Order.Gate

data OrderRep o e = OrderRep (RawAlgorithm RealWorld o e) (Gate o)
{-NOTE:
    When using OrderT, reduction of an OrderRep value to WHNF triggers the I/O
    for insertions.
-}

newOrderRep :: RawAlgorithm RealWorld o e -> IO (OrderRep o e)
newOrderRep rawAlg = do
    rawOrder <- stToIO $ newOrder rawAlg
    gate <- newGate rawOrder
    return (OrderRep rawAlg gate)
