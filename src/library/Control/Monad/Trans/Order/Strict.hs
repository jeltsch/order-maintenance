module Control.Monad.Trans.Order.Strict (

    -- * The Order monad

    Order,
    perform,

    -- * The OrderT monad transformer

    OrderT,
    performT,
    getOrderToken,

    -- * Element creation

    newMinimum,
    newMaximum,
    newAfter,
    newBefore,

    -- * Converting between lazy and strict OrderT

    lazyToStrictOrderT,
    strictToLazyOrderT

) where

-- Control

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Class (MonadTrans)
import qualified Control.Monad.Trans.Class as Trans (lift)
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.State.Strict as Strict
import           Control.Monad.Trans.Order.Representation
                     (OrderTRep (OrderTRep), StateMonadTrans (..))
import qualified Control.Monad.Trans.Order.Representation as OrderTRep
import qualified Control.Monad.Trans.Order.Lazy.Type as Lazy (OrderT (OrderT))

-- Data

import Data.Functor.Identity
import Data.Order.Pair.Type
import Data.Order.Element

{-FIXME:
    Consider introducing a restricted variant of mapStateT:

            mapOrderT :: (forall a . m a -> n a) -> OrderT o m a -> OrderT o n a

    Maybe this should not be called mapOrderT, since it is only a restricted
    variant and a corresponding mapOrder would be trivial.
-}
{-FIXME:
    Probably we should also have variants of liftCallCC, etc., which are present
    for StateT.
-}

-- * The Order monad

type Order o = OrderT o Identity

perform :: (a -> Order o b) -> OrderPair o a -> OrderPair o b
perform fun pair = runIdentity (performT fun pair)

-- * The OrderT monad transformer

newtype OrderT o m a = OrderT {
            runOrderT :: OrderTRep Strict.StateT o m a
        } deriving (
            Functor,
            Applicative,
            Alternative,
            Monad,
            MonadPlus,
            MonadFix,
            MonadTrans,
            MonadIO
        )

instance StateMonadTrans Strict.StateT where

    -- Construction and destruction

    stateT = Strict.StateT

    runStateT = Strict.runStateT

    -- Functor

    fmap' = fmap

    (<$!) = (<$)

    -- Applicative

    pure' = pure

    (<*>!) = (<*>)

    (*>!) = (*>)

    (<*!) = (<*)

    -- Alternative

    empty' = empty

    (<|>!) = (<|>)

    some' = some

    many' = many

    -- Monad

    (>>=!) = (>>=)

    (>>!) = (>>)

    return' = return

    fail' = fail

    -- MonadPlus

    mzero' = mzero

    mplus' = mplus

    -- MonadFix

    mfix' = mfix

    -- MonadTrans

    lift' = Trans.lift

    -- MonadIO

    liftIO' = liftIO

performT :: Functor f
         => (a -> OrderT o f b)
         -> OrderPair o a
         -> f (OrderPair o b)
performT fun (OrderPair (val, orderRep)) = output where

    output = OrderTRep.performT (runOrderT . fun) val orderRep

getOrderToken :: Applicative f => OrderT o f ()
getOrderToken = OrderT $ OrderTRep.getOrderToken

lift :: Functor f => f a -> OrderT o f a
lift struct = OrderT $ OrderTRep.lift struct
{-NOTE:
    This version is more general than the one from MonadTrans, since it works
    with arbitrary functors, not just monads.
-}

-- * Element creation

newMinimum :: Applicative f => OrderT o f (Element o)
newMinimum = OrderT $ OrderTRep.newMinimum

newMaximum :: Applicative f => OrderT o f (Element o)
newMaximum = OrderT $ OrderTRep.newMaximum

newAfter :: Applicative f => Element o -> OrderT o f (Element o)
newAfter elem = OrderT $ OrderTRep.newAfter elem

newBefore :: Applicative f => Element o -> OrderT o f (Element o)
newBefore elem = OrderT $ OrderTRep.newBefore elem

-- * Converting between lazy and strict OrderT

lazyToStrictOrderT :: Lazy.OrderT o m a -> OrderT o m a
lazyToStrictOrderT (Lazy.OrderT (OrderTRep comp)) = strictOrderT where

    strictOrderT = OrderT $ OrderTRep $ stateT (runStateT comp)

strictToLazyOrderT :: OrderT o m a -> Lazy.OrderT o m a
strictToLazyOrderT (OrderT (OrderTRep comp)) = lazyOrderT where

    lazyOrderT = Lazy.OrderT $ OrderTRep $ stateT (runStateT comp)
