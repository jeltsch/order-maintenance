module Control.Monad.Trans.Order (

    -- * The Order monad

    Order,
    runOrder,
    toOrderComp,
    evalOrder,

    -- * The OrderT monad transformer

    OrderT (OrderT),
    runOrderT,
    toOrderCompT,
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

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class

-- Data

import Data.Functor.Identity
import Data.OrderMaintenance

{-FIXME:
    The monads WriterT, StateT, and RWST come in a lazy and a strict variant,
    where the terms “lazy monad” and “strict monad” are defined as in the
    documentation of Control.Monad.Trans.Class. These lazy and strict variants
    differ in how (>>=) is implemented. ReaderT is always a lazy monad, and
    ContT is always a strict monad. As a result of the latter fact, there is no
    lazy and strict variant of OrderT, but OrderT is always a strict monad.

    A different matter is lazyness and strictness in the order. The analogous
    notion for StateT is lazyness and strictness in the state. In the case of
    StateT, it is possible to explicitely force the state using the computation
    get >>= \ s -> s `seq` (). For OrderT, we provide force.
-}
{-FIXME:
    Big problem:

      • OrderT is strict in the sense of the transformers package. As a result,
        the conversion from the CPS representation to the pure function
        representation in the incremental-computing package will fail.

    Possible solution:

      • Ditch OrderCompT and implement OrderT o as StateT (Order o) in lazy and
        strict variants.

    When following this solution, consider introducing a restricted variant of
    mapStateT:

            mapOrderT :: (forall a . m a -> n a) -> OrderT o m a -> OrderT o n a
-}

-- * The Order monad

type Order o = OrderT o Identity

runOrder :: Order o a -> Cont (OrderComp o r) a
runOrder = runOrderT

toOrderComp :: Order o a -> OrderComp o a
toOrderComp = toOrderCompT

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
        cps _ = withOutputOf (MaybeT (writer (Nothing, [()]))) finish

    Note that the mempty of SpecificComp r is as follows:

        withOutputOf (MaybeT (writer (Nothing, []))) finish
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

force :: OrderT o m ()
force = OrderT $ cont (withForcedOrder . ($ ()))

-- * Elements

-- NOTE: Element is imported from Data.OrderMaintenance.

newMinimum :: OrderT o m (Element o)
newMinimum = OrderT $ cont withNewMinimum

newMaximum :: OrderT o m (Element o)
newMaximum = OrderT $ cont withNewMinimum

newAfter :: Element o -> OrderT o m (Element o)
newAfter refElem = OrderT $ cont (withNewAfter refElem)

newBefore :: Element o -> OrderT o m (Element o)
newBefore refElem = OrderT $ cont (withNewAfter refElem)

{-NOTE:
    OrderT o is a monad. As a result, it is also an applicative functor, but one
    that is about sequential execution. Comparison of elements generated by
    different subcomputations combined by (<*>) is not a problem, as these
    subcomputations are sequentially executed and so the order is determined.
-}
