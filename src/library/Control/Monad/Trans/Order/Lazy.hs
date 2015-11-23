module Control.Monad.Trans.Order.Lazy (

    -- * The Order monad

    Order,
    evalOrder,
    evalOrderWith,

    -- * The OrderT monad transformer

    OrderT,
    evalOrderT,
    evalOrderTWith,
    force,

    -- * Elements

    newMinimum,
    newMaximum,
    newAfter,
    newBefore

) where

-- Control

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Order.Lazy.Type

-- Data

import           Data.Functor.Identity
import           Data.Order.Algorithm
import           Data.Order.Algorithm.Type
import           Data.Order.Internals
                 hiding (newMinimum, newMaximum, newAfter, newBefore)
import qualified Data.Order.Internals as Internals
import           Data.Order.Raw (RawAlgorithm)

-- System

import System.IO.Unsafe

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

-- NOTE: OrderT is imported from Control.Monad.Trans.Order.Lazy.Type.

evalOrderT :: Monad m => (forall o . OrderT o m a) -> m a
evalOrderT = evalOrderTWith defaultAlgorithm

evalOrderTWith :: Monad m => Algorithm -> (forall o . OrderT o m a) -> m a
evalOrderTWith (Algorithm rawAlg) (OrderT stateT) = monad where

    monad = evalStateT stateT (localOrderRep rawAlg)

force :: Monad m => OrderT o m ()
force = OrderT $ get >>= \ order -> order `seq` return ()

-- * Elements

newMinimum :: Monad m => OrderT o m (Element o)
newMinimum = fromRepNew Internals.newMinimum

newMaximum :: Monad m => OrderT o m (Element o)
newMaximum = fromRepNew Internals.newMaximum

newAfter :: Monad m => Element o -> OrderT o m (Element o)
newAfter elem = fromRepNew (Internals.newAfter elem)

newBefore :: Monad m => Element o -> OrderT o m (Element o)
newBefore elem = fromRepNew (Internals.newBefore elem)

fromRepNew :: Monad m
           => (OrderRep o -> IO (Element o))
           -> OrderT o m (Element o)
fromRepNew repNew = OrderT $ state statefulNew where

    statefulNew orderRep = (elem, elem `seq` orderRep) where

        {-# NOINLINE elem #-}
        elem = unsafePerformIO $ repNew orderRep
