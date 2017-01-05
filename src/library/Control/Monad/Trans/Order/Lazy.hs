module Control.Monad.Trans.Order.Lazy (

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
    newBefore

) where

-- Control

import           Control.Applicative
import           Control.Monad.Trans.Order.Lazy.Type
import qualified Control.Monad.Trans.Order.Representation as OrderTRep

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

-- NOTE: OrderT is imported from Control.Trans.Order.Lazy.Type.

performT :: Functor f
         => (a -> OrderT o f b)
         -> OrderPair o a
         -> f (OrderPair o b)
performT fun (OrderPair ~(val, orderRep)) = output where

    output = OrderTRep.performT (runOrderT . fun) val orderRep

getOrderToken :: Applicative f => OrderT o f ()
getOrderToken = OrderT $ OrderTRep.getOrderToken

-- * Element creation

newMinimum :: Applicative f => OrderT o f (Element o)
newMinimum = OrderT $ OrderTRep.newMinimum

newMaximum :: Applicative f => OrderT o f (Element o)
newMaximum = OrderT $ OrderTRep.newMaximum

newAfter :: Applicative f => Element o -> OrderT o f (Element o)
newAfter elem = OrderT $ OrderTRep.newAfter elem

newBefore :: Applicative f => Element o -> OrderT o f (Element o)
newBefore elem = OrderT $ OrderTRep.newBefore elem
