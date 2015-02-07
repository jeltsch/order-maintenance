module Control.Monad.Trans.Order.Strict (

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

    -- * Converting between lazy and strict OrderT

    lazyToStrictOrderT,
    strictToLazyOrderT

) where

-- Control

import           Control.Monad.Trans.State.Lazy                     as Lazy
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Order.Lazy (OrderRep, Element)
import qualified Control.Monad.Trans.Order.Lazy                     as Lazy
import qualified Control.Monad.Trans.Order.Lazy.Internals           as Lazy

-- * The Order monad

type Order o = OrderT o Identity

evalOrder :: (forall o . Order o a) -> a
evalOrder order = runIdentity (evalOrderT order)

evalOrderWith :: Algorithm -> (forall o . Order o a) -> a
evalOrderWith alg order = runIdentity (evalOrderTWith alg order)

-- * The OrderT monad transformer

newtype OrderT o m a = OrderT (StateT (OrderRep o) m a) deriving (
    Functor,
    Applicative,
    Alternative,
    Monad,
    MonadPlus,
    MonadTrans,
    MonadIO)
    -- FIXME: Should we also have a MonadFix instance?

evalOrderT :: Monad m => (forall o . OrderT o m a) -> m a
evalOrderT = evalOrderTWith defaultAlgorithm

evalOrderTWith :: Monad m => Algorithm -> (forall o . OrderT o m a) -> m a
evalOrderTWith (Algorithm rawAlg) (OrderT stateT) = monad where

    monad = evalStateT stateT (emptyOrderRep rawAlg)

force :: Monad m => OrderT o m ()
force = lazyToStrictOrderT Lazy.force

-- * Elements

newMinimum :: Monad m => OrderT o m (Element o)
newMinimum = lazyToStrictOrderT Lazy.newMinimum

newMaximum :: Monad m => OrderT o m (Element o)
newMaximum = lazyToStrictOrderT Lazy.newMaximum

newAfter :: Monad m => Element o -> OrderT o m (Element o)
newAfter = lazyToStrictOrderT . Lazy.newAfter

newBefore :: Monad m => Element o -> OrderT o m (Element o)
newBefore = lazyToStrictOrderT . Lazy.newBefore

-- * Converting between lazy and strict OrderT

lazyToStrictOrderT :: Lazy.OrderT o m a -> OrderT o m a
lazyToStrictOrderT (Lazy.OrderT (Lazy.StateT fun)) = OrderT (StateT fun)

strictToLazyOrderT :: OrderT o m a -> Lazy.OrderT o m a
strictToLazyOrderT (OrderT (StateT fun)) = Lazy.OrderT (Lazy.StateT fun)
