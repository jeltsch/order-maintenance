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

-- Control

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class

-- Data

import Data.Monoid
import Data.Functor.Identity
import Data.OrderMaintenance

{-FIXME:
    For WriterT, StateT, and RWST, there are strict and lazy variants. They
    differ in how (>>=) is implemented. ReaderT is always lazy, and ContT is
    always strict. As a result of the latter, there is no strict and lazy
    variant of OrderT, but OrderT is always a strict monad in the sense of the
    Control.Monad.Trans.Class documentation.

    We should distinguish between lazy and strict versions of the insert
    operations. Is this about strictness in the order? Is there actually a
    difference, given that ContT is always strict?

    Maybe we do not even need lazy and strict versions of the insert operations,
    also not of the primitive ones for OrderCompT. It might be enough to have a
    operation that forces the order. For OrderT, this would be analogous to the
    operation get >>= \ s -> s `seq` () for StateT. For OrderCompT, this
    operation would probably have the type OrderCompT -> OrderCompT and be
    implemented as ($!).
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
    A type-correct MonadPlus instance could be constructed for ContT if the
    inner monad is an instance of MonadPlus. However, this instance would not
    fulfill the law

        _ >> mzero = mzero

    in general. It would do so only if the first argument of (>>) was a CPS
    computation that “respects mzero“ in the sense that applying this CPS
    computation to the continuation const mzero yielded mzero.

    The case for OrderT is similar. A type-correct MonadPlus instance could be
    constructed in terms of the OrderCompT Monoid instance. However, it would
    fulfill the above law only if applying the first argument of (>>) to the
    continuation const mempty yielded mempty. This is not always the case,
    though. A counterexample is the following CPS:

        type SpecificComp = OrderCompT o (MaybeT (Writer [()]))

        cps :: (a -> SpecificComp r) -> SpecificComp r
        cps _ = withOutputOf (MaybeT (writer (Nothing,[()]))) finish

    Note that the mempty of SpecificComp r is as follows:

        withOutputOf (MaybeT (writer (Nothing,[]))) finish
-}

{-FIXME:
    OrderT probably cannot be an instance of MonadFix. ContT is not either.
    However, see the the discussion at <http://tinyurl.com/p5az2km>.
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
