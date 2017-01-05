module Data.Order.Pair (

    OrderPair,
    empty,
    emptyBy,
    companion

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

empty :: OrderPair o ()
empty = emptyBy defaultAlgorithm

emptyBy :: Algorithm -> OrderPair o ()
emptyBy (Algorithm rawAlg) = OrderPair ((), emptyOrderRepBy rawAlg)

{-# NOINLINE emptyOrderRepBy #-}
emptyOrderRepBy :: RawAlgorithm RealWorld o e -> OrderRep o e
emptyOrderRepBy rawAlg = unsafePerformIO $ newOrderRep rawAlg

companion :: (forall o . OrderPair o a) -> a
companion (OrderPair (comp, _)) = comp
