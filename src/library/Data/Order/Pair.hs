module Data.Order.Pair (

    OrderPair,
    emptyOrderPair,
    emptyOrderPairUsing,
    withoutOrder

) where

-- Control

import Control.Monad.ST

-- Data

import Data.Order.Pair.Type
import Data.Order.Representation
import Data.Order.Algorithm
import Data.Order.Algorithm.Type
import Data.Order.Algorithm.Raw

-- System

import System.IO.Unsafe

-- NOTE: OrderPair is imported from Data.Order.Pair.Type.

emptyOrderPair :: a -> OrderPair o a
emptyOrderPair = emptyOrderPairUsing defaultAlgorithm

emptyOrderPairUsing :: Algorithm -> a -> OrderPair o a
emptyOrderPairUsing (Algorithm rawAlg) val = orderPair where

    orderPair = OrderPair (val, emptyOrderRepUsing rawAlg)

{-# NOINLINE emptyOrderRepUsing #-}
emptyOrderRepUsing :: RawAlgorithm RealWorld o e -> OrderRep o e
emptyOrderRepUsing rawAlg = unsafePerformIO $ newOrderRep rawAlg

withoutOrder :: (forall o . OrderPair o a) -> a
withoutOrder (OrderPair (val, _)) = val
