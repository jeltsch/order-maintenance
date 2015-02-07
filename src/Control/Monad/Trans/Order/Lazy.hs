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

import Control.Monad.ST
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Order.Raw
import Control.Monad.Trans.Order.Algorithm
import Control.Monad.Trans.Order.Algorithm.Type
import Control.Monad.Trans.Order.Lazy.Internals

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

evalOrderT :: Monad m => (forall o . OrderT o m a) -> m a
evalOrderT = evalOrderTWith defaultAlgorithm

evalOrderTWith :: Monad m => Algorithm -> (forall o . OrderT o m a) -> m a
evalOrderTWith (Algorithm rawAlg) (OrderT stateT) = monad where

    monad = evalStateT stateT (emptyOrderRep rawAlg)

force :: Monad m => OrderT o m ()
force = OrderT $ get >>= \ order -> order `seq` return ()

-- * Elements

data Element o = Element (RawElement o RealWorld)
                         (RawAlgorithm o RealWorld)
                         Lock
-- NOTE: Evaluation of the Element constructor triggers the I/O for insertions.

instance Eq (Element o) where

    (==) (Element rawElem1 (RawAlgorithm _ _ _ _ _ _ _) _)
         (Element rawElem2 _                            _) = equal where

        equal = rawElem1 == rawElem2

instance Ord (Element o) where

    compare (Element rawElem1 rawAlg lock)
            (Element rawElem2 _      _)    = ordering where

        ordering = unsafePerformIO $
                   criticalSection lock $
                   stToIO $ compareElements rawAlg rawElem1 rawElem2
{-FIXME:
    Introduce the safety measures for unsafePerformIO. It should not matter how
    many times the I/O is performed.
-}

fromInsert :: Monad m
           => (RawAlgorithm o RealWorld
                   -> RawOrder o RealWorld
                   -> ST RealWorld (RawElement o RealWorld))
           -> OrderT o m (Element o)
fromInsert insert = OrderT $ StateT (return . explicitStateInsert) where

    explicitStateInsert order@(OrderRep rawOrder rawAlg lock) = output where

        output = unsafePerformIO $
                 criticalSection lock $
                 do
                     rawElem <- stToIO $ insert rawAlg rawOrder
                     mkWeakIORef (IORef rawElem)
                                 (criticalSection lock $
                                  stToIO $
                                  delete rawAlg rawElem rawOrder)
                     return (Element rawElem rawAlg lock, order)
    {-FIXME:
        Introduce the safety measures for unsafePerformIO. The I/O must occur only
        once.
    -}

newMinimum :: Monad m => OrderT o m (Element o)
newMinimum = fromInsert insertMinimum

newMaximum :: Monad m => OrderT o m (Element o)
newMaximum = fromInsert insertMaximum

newAfter :: Monad m => Element o -> OrderT o m (Element o)
newAfter (~(Element rawElem _ _)) = fromInsert (flip insertAfter rawElem)

newBefore :: Monad m => Element o -> OrderT o m (Element o)
newBefore (~(Element rawElem _ _)) = fromInsert (flip insertBefore rawElem)
