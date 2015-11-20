module Data.Order.Internals (

    -- * Order representations

    OrderRep (OrderRep),
    newOrderRep,
    emptyOrderRep,

    -- * Elements

    Element (Element),
    newMinimum,
    newMaximum,
    newAfter,
    newBefore

) where

-- Control

import Control.Monad.ST
import Control.Concurrent.MVar
import Control.Exception

-- Data

import           Data.IORef
import           Data.Order.Raw
                 hiding (newMinimum, newMaximum, newAfter, newBefore)
import qualified Data.Order.Raw as Raw

-- System

import System.IO.Unsafe

-- GHC

import GHC.IORef -- for converting from STRef RealWorld to IORef

-- * Order representations

data OrderRep o = OrderRep (RawAlgorithm o RealWorld) (Gate o)
-- FIXME: Maybe use OrderedSet instead of OrderToken.
{-NOTE:
    When using OrderT, evaluation of the OrderRep constructor triggers the I/O
    for insertions.
-}

newOrderRep :: (forall s . RawAlgorithm o s) -> IO (OrderRep o)
newOrderRep rawAlg = do
    rawOrder <- stToIO $ Raw.newOrder rawAlg
    gate <- newGate rawOrder
    return (OrderRep rawAlg gate)

emptyOrderRep :: (forall s . RawAlgorithm o s) -> OrderRep o
emptyOrderRep rawAlg = unsafePerformIO $ newOrderRep rawAlg
{-FIXME:
    Introduce the safety measures for unsafePerformIO. It should not matter how
    many times the I/O is performed, as emptyOrderRep is only used in the OrderT
    implementation.
-}

-- * Elements

data Element o = Element (RawAlgorithm o RealWorld)
                         (Gate o)
                         (RawElement o RealWorld)
{-NOTE:
    When using OrderT, evaluation of the Element constructor triggers the I/O
    for insertions.
-}

instance Eq (Element o) where

    (==) (Element (RawAlgorithm _ _ _ _ _ _ _) _ rawElem1)
         (Element _                            _ rawElem2) = equal where

        equal = rawElem1 == rawElem2

instance Ord (Element o) where

    compare (Element rawAlg gate rawElem1)
            (Element _      _    rawElem2) = ordering where

        ordering = unsafePerformIO $
                   withRawOrder gate $ \ rawOrder ->
                   stToIO $ compareElements rawAlg rawOrder rawElem1 rawElem2
{-FIXME:
    Introduce the safety measures for unsafePerformIO. It should not matter how
    many times the I/O is performed.
-}

newMinimum :: OrderRep o -> IO (Element o)
newMinimum = fromRawNew Raw.newMinimum

newMaximum :: OrderRep o -> IO (Element o)
newMaximum = fromRawNew Raw.newMaximum

newAfter :: Element o -> OrderRep o -> IO (Element o)
newAfter = fromRawNeighbor Raw.newAfter

newBefore :: Element o -> OrderRep o -> IO (Element o)
newBefore = fromRawNeighbor Raw.newBefore

fromRawNeighbor :: (RawAlgorithm o RealWorld
                        -> RawOrder o RealWorld
                        -> RawElement o RealWorld
                        -> ST RealWorld (RawElement o RealWorld))
                -> Element o
                -> OrderRep o
                -> IO (Element o)
fromRawNeighbor rawNewNeighbor (Element _ _ rawElem) = fromRawNew rawNew where

    rawNew rawAlg rawOrder = rawNewNeighbor rawAlg rawOrder rawElem

fromRawNew :: (RawAlgorithm o RealWorld
                   -> RawOrder o RealWorld
                   -> ST RealWorld (RawElement o RealWorld))
           -> OrderRep o
           -> IO (Element o)
fromRawNew rawNew (OrderRep rawAlg gate) = withRawOrder gate $ \ rawOrder -> do
    rawElem <- stToIO $ rawNew rawAlg rawOrder
    mkWeakIORef (IORef rawElem)
                (withRawOrder gate $ \ rawOrder ->
                 stToIO $
                 delete rawAlg rawOrder rawElem)
    return (Element rawAlg gate rawElem)

-- * Gates

newtype Gate a = Gate (MVar (RawOrder a RealWorld))

newGate :: RawOrder a RealWorld -> IO (Gate a)
newGate = fmap Gate . newMVar

withRawOrder :: Gate a -> (RawOrder a RealWorld -> IO r) -> IO r
withRawOrder (Gate mVar) cont = bracket (takeMVar mVar) (putMVar mVar) cont
