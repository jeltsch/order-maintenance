module Control.Monad.Trans.Order.Lazy (

    -- * The Order monad

    Order,
    evalOrder,
    evalOrderWith,

    -- * The OrderT monad transformer

    OrderT,
    evalOrderT,
    force,

    -- * Elements

    Element,
    newMinimum,
    newMaximum,
    newAfter,
    newBefore

) where

-- Control

import           Control.Monad.ST
import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Trans.Order.Raw
                     hiding (newMinimum, newMaximum, newAfter, newBefore)
import qualified Control.Monad.Trans.Order.Raw
                     as Raw
import           Control.Monad.Trans.Order.Lazy.Internals
import           Control.Monad.Trans.Order.Algorithm
import           Control.Monad.Trans.Order.Algorithm.Type

-- Data

import Data.Functor.Identity
import Data.IORef

-- System

import System.IO.Unsafe

-- GHC

import GHC.IORef -- for converting from STRef RealWorld to IORef

{-FIXME:
    Introduce conversions between the lazy and the strict variant, similar to
    the conversions for ST.
-}
{-FIXME:
    Consider introducing a restricted variant of mapStateT (for the lazy and the
    strict OrderT monad):

            mapOrderT :: (forall a . m a -> n a) -> OrderT o m a -> OrderT o n a

    Maybe this should not be called mapOrderT, since it is only a restricted
    variant and a corresponding mapOrder would be trivial.
-}
{-FIXME:
    Probably we should also have variants of liftCallCC, etc., which are present
    for StateT (for the lazy and the strict OrderT monad).
-}

-- * The Order monad

type Order o = OrderT o Identity

evalOrder :: (forall o . Order o a) -> a
evalOrder order = runIdentity (evalOrderT order)

evalOrderWith :: Algorithm -> (forall o . Order o a) -> a
evalOrderWith alg order = runIdentity (evalOrderTWith alg order)

-- * The OrderT monad transformer

-- NOTE: OrderT is imported from Control.Monad.Trans.Order.Lazy.Internals.

evalOrderT :: Monad m => (forall o . OrderT o m a) -> m a
evalOrderT = evalOrderTWith defaultAlgorithm

evalOrderTWith :: Monad m => Algorithm -> (forall o . OrderT o m a) -> m a
evalOrderTWith (Algorithm rawAlg) (OrderT stateT) = monad where

    monad = evalStateT stateT (emptyOrderRep rawAlg)

force :: Monad m => OrderT o m ()
force = OrderT $ get >>= \ order -> order `seq` return ()

-- * Elements

data Element o = Element (RawAlgorithm o RealWorld)
                         (Gate o)
                         (RawElement o RealWorld)
-- NOTE: Evaluation of the Element constructor triggers the I/O for insertions.

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

fromRawNew :: Monad m
           => (RawAlgorithm o RealWorld
                   -> RawOrder o RealWorld
                   -> ST RealWorld (RawElement o RealWorld))
           -> OrderT o m (Element o)
fromRawNew rawNew = OrderT $ StateT (return . explicitStateNew) where

    explicitStateNew order@(OrderRep rawAlg gate) = output where

        output = unsafePerformIO $
                 withRawOrder gate $ \ rawOrder ->
                 do
                     rawElem <- stToIO $ rawNew rawAlg rawOrder
                     mkWeakIORef (IORef rawElem)
                                 (withRawOrder gate $ \ rawOrder ->
                                  stToIO $
                                  delete rawAlg rawOrder rawElem)
                     return (Element rawAlg gate rawElem, order)
    {-FIXME:
        Introduce the safety measures for unsafePerformIO. The I/O must occur only
        once.
    -}

newMinimum :: Monad m => OrderT o m (Element o)
newMinimum = fromRawNew Raw.newMinimum

newMaximum :: Monad m => OrderT o m (Element o)
newMaximum = fromRawNew Raw.newMaximum

newAfter :: Monad m => Element o -> OrderT o m (Element o)
newAfter (~(Element _ _ rawElem)) = fromRawNeighbor Raw.newAfter rawElem

newBefore :: Monad m => Element o -> OrderT o m (Element o)
newBefore (~(Element _ _ rawElem)) = fromRawNeighbor Raw.newBefore rawElem

fromRawNeighbor :: Monad m
                => (RawAlgorithm o RealWorld
                        -> RawOrder o RealWorld
                        -> RawElement o RealWorld
                        -> ST RealWorld (RawElement o RealWorld))
                -> RawElement o RealWorld
                -> OrderT o m (Element o)
fromRawNeighbor rawNewNeighbor rawElem = fromRawNew rawNew where

    rawNew rawAlg rawOrder = rawNewNeighbor rawAlg rawOrder rawElem
