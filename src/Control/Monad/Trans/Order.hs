module Control.Monad.Trans.Order (

    -- * The Order monad
    Order,
    evalOrder,

    -- * The OrderT monad transformer
    OrderT (OrderT),
    runOrderT,
    toOrderCompT,
    evalOrderT,
    insertMinimum,
    insertMaximum,
    insertAfter,
    insertBefore

) where

-- * Control
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class

-- * Data
import Data.Functor.Identity
import Data.OrderMaintenance

{-FIXME:
    We have to also introduce a strict variant of Order and OrderT. The strict
    and lazy variants should go into respective submodules, and this submodule
    should re-export the lazy submodule, analogously to RWS, State, and Writer.
-}

-- * The Order monad

type Order o = OrderT o Identity

evalOrder :: (forall o . Order o a) -> a
evalOrder order = runIdentity (evalOrderT order)

-- * The OrderT monad transformer

newtype OrderT o m a = OrderT (forall r . Cont (OrderCompT o m r) a)

instance Functor (OrderT o m) where

    fmap fun (OrderT cont) = OrderT $ fmap fun cont

instance Applicative (OrderT o m) where

    pure val = OrderT (pure val)

    OrderT funCont <*> OrderT argCont = OrderT $ funCont <*> argCont

instance Monad (OrderT o m) where

    return val = OrderT $ return val

    OrderT cont1 >>= ot2Gen = OrderT $
                              cont1 >>= \ val1 -> let

                                                      OrderT cont2 = ot2Gen val1
                                                       
                                                  in cont2

{-FIXME:
    Implement also instances of Alternative and MonadPlus. These cannot be taken
    from the underlying Cont, as it does not have such instances. However, it
    should be possible to define operations on orderCompT based on
    composeOrderCompT similarly to defining finish and branch, and then use
    these operations to define the monoidal structure of OrderT.
-}

{-FIXME:
    Can we also have a MonadFix instance? ContT does not have any, but maybe
    OrderT is special enough to allow for one.
-}

instance MonadTrans (OrderT o) where

    lift monad = OrderT $ cont (withOutputOf monad)

instance MonadIO m => MonadIO (OrderT o m) where

    liftIO = lift . liftIO

runOrderT :: OrderT o m a -> Cont (OrderCompT o m r) a
runOrderT (OrderT cont) = cont

toOrderCompT :: Applicative m => OrderT o m a -> OrderCompT o m a
toOrderCompT (OrderT cont) = runCont cont finish

evalOrderT :: Applicative m => (forall o . OrderT o m a) -> m a
evalOrderT orderT = evalOrderCompT (toOrderCompT orderT)

insertMinimum :: OrderT o m (Element o)
insertMinimum = OrderT $ cont withNewMinimum

insertMaximum :: OrderT o m (Element o)
insertMaximum = OrderT $ cont withNewMinimum

insertAfter :: Element o -> OrderT o m (Element o)
insertAfter refElem = OrderT $ cont (withNewAfter refElem)

insertBefore :: Element o -> OrderT o m (Element o)
insertBefore refElem = OrderT $ cont (withNewAfter refElem)

{-NOTE:
    OrderT o is a monad. As a result, it is also an applicative functor, but one
    that is about sequential execution. Comparison of elements generated by
    different subcomputations combined by (<*>) is not a problem, as these
    subcomputations are sequentially executed and so the order is determined.
-}
