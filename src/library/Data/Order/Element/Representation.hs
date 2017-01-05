module Data.Order.Element.Representation (

    ElementRep (ElementRep),
    newMinimum,
    newMaximum,
    newAfter,
    newBefore

) where

-- Control

import Control.Monad.ST

-- Data

import           Data.Order.Representation
import           Data.Order.Algorithm.Raw (RawOrder, RawElement, RawAlgorithm)
import qualified Data.Order.Algorithm.Raw as Raw
import           Data.Order.Gate
import           Data.IORef

-- System

import System.IO.Unsafe

-- GHC

import GHC.IORef (IORef (IORef))

data ElementRep o e = ElementRep (RawAlgorithm RealWorld o e)
                                 (Gate o)
                                 (RawElement RealWorld e)
{-NOTE:
    When using OrderT, reduction of an ElementRep value to WHNF triggers the I/O
    for insertions.
-}

instance Eq (ElementRep o e) where

    ElementRep _ _ rawElem1  == ElementRep _ _ rawElem2 = rawElem1 == rawElem2

instance Ord (ElementRep o e) where

    compare (ElementRep rawAlg gate rawElem1)
            (ElementRep _      _    rawElem2) = ordering where
            
        ordering = unsafePerformIO $
                   withRawOrder gate $ \ rawOrder ->
                       stToIO $
                       Raw.compareElements rawAlg rawElem1 rawElem2 rawOrder

newMinimum :: OrderRep o e -> IO (ElementRep o e)
newMinimum = fromRawNew Raw.newMinimum

newMaximum :: OrderRep o e -> IO (ElementRep o e)
newMaximum = fromRawNew Raw.newMaximum

newAfter :: ElementRep o e -> OrderRep o e -> IO (ElementRep o e)
newAfter = fromRawNewNeighbor Raw.newAfter

newBefore :: ElementRep o e -> OrderRep o e -> IO (ElementRep o e)
newBefore = fromRawNewNeighbor Raw.newBefore

fromRawNewNeighbor :: (RawAlgorithm RealWorld o e ->
                       RawElement RealWorld e     ->
                       RawOrder RealWorld o       ->
                       ST RealWorld (RawElement RealWorld e))
                   -> ElementRep o e
                   -> OrderRep o e
                   -> IO (ElementRep o e)
fromRawNewNeighbor rawNewNeighbor (ElementRep _ _ rawElem) = fromRawNew rawNew where

    rawNew rawAlg = rawNewNeighbor rawAlg rawElem

fromRawNew :: (RawAlgorithm RealWorld o e ->
               RawOrder RealWorld o       ->
               ST RealWorld (RawElement RealWorld e))
           -> OrderRep o e
           -> IO (ElementRep o e)
fromRawNew rawNew (OrderRep rawAlg gate) = withRawOrder gate $ \ rawOrder -> do
    rawElem <- stToIO $ rawNew rawAlg rawOrder
    mkWeakIORef (IORef rawElem)
                (withRawOrder gate $ \ rawOrder ->
                     stToIO $
                     Raw.delete rawAlg rawElem rawOrder)
    return (ElementRep rawAlg gate rawElem)
