module Control.Monad.Trans.Order.Representation (

    -- * The OrderTRep monad transformer

    OrderTRep (OrderTRep),
    performT,
    getOrderToken,

    -- * Element creation

    newMinimum,
    newMaximum,
    newAfter,
    newBefore,

    -- * State monad transformers

    state,
    StateMonadTrans (..)

) where

-- Control

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- Data

import           Data.Order.Pair.Type
import           Data.Order.Element.Type
import           Data.Order.Representation
import           Data.Order.Element.Representation (ElementRep)
import qualified Data.Order.Element.Representation as ElementRep

-- System

import System.IO.Unsafe

-- Unsafe

import Unsafe.Coerce

-- Fixities

infixl 4 <$!
infixl 4 <*>!
infixl 4 *>!
infixl 4 <*!
infixl 3 <|>!
infixl 1 >>=!
infixl 1 >>!

-- * The OrderTRep monad transformer

newtype OrderTRep t o (m :: * -> *) a = OrderTRep {
    runOrderTRep :: forall o' e' . t (OrderRep o' e') m a
}
{-NOTE:
    There are two type variables named o. The parameter o of OrderT is the tag
    that is used to make the user interface safe. The universally quantified
    variable o is the type of order cells used by the respective algorithm.
-}

instance (StateMonadTrans t, Functor f) => Functor (OrderTRep t o f) where

    fmap fun (OrderTRep comp) = OrderTRep $ fmap' fun comp

    val <$ OrderTRep comp = OrderTRep $ val <$! comp

instance (StateMonadTrans t, Monad f) => Applicative (OrderTRep t o f) where

    pure val = OrderTRep $ pure' val

    OrderTRep funComp <*> OrderTRep valComp = OrderTRep $ funComp <*>! valComp

    OrderTRep comp1 *> OrderTRep comp2 = OrderTRep $ comp1 *>! comp2

    OrderTRep comp1 <* OrderTRep comp2 = OrderTRep $ comp1 <*! comp2

instance (StateMonadTrans t, MonadPlus m) => Alternative (OrderTRep t o m) where

    empty = OrderTRep $ empty'

    OrderTRep comp1 <|> OrderTRep comp2 = OrderTRep $ comp1 <|>! comp2

    some (OrderTRep comp) = OrderTRep $ some' comp

    many (OrderTRep comp) = OrderTRep $ many' comp

instance (StateMonadTrans t, Monad m) => Monad (OrderTRep t o m) where

    OrderTRep comp >>= fun = OrderTRep $ comp >>=! runOrderTRep . fun

    OrderTRep comp1 >> OrderTRep comp2 = OrderTRep $ comp1 >>! comp2

    return val = OrderTRep $ return' val

    fail msg = OrderTRep $ fail' msg

instance (StateMonadTrans t, MonadPlus m) => MonadPlus (OrderTRep t o m) where

    mzero = OrderTRep $ mzero'

    mplus (OrderTRep comp1) (OrderTRep comp2) = OrderTRep $ mplus' comp1 comp2

instance (StateMonadTrans t, MonadFix m) => MonadFix (OrderTRep t o m) where

    mfix fun = OrderTRep $ mfix' (runOrderTRep . fun)

instance StateMonadTrans t => MonadTrans (OrderTRep t o) where

    lift struct = OrderTRep $ lift' struct

instance (StateMonadTrans t, MonadIO m) => MonadIO (OrderTRep t o m) where

    liftIO io = OrderTRep $ liftIO' io

performT :: (StateMonadTrans t, Functor f)
         => (a -> OrderTRep t o f b)
         -> a
         -> OrderRep o' e'
         -> f (OrderPair o b)
performT fun val orderRep = OrderPair <$> struct where

    struct = (runStateT $ runOrderTRep $ fun val) orderRep

getOrderToken :: (StateMonadTrans t, Applicative f) => OrderTRep t o f ()
getOrderToken = OrderTRep $ state $ \ orderRep -> (orderRep `seq` (), orderRep)

-- * Element creation

newMinimum :: (StateMonadTrans t, Applicative f)
           => OrderTRep t o f (Element o)
newMinimum = fromRepNew ElementRep.newMinimum

newMaximum :: (StateMonadTrans t, Applicative f)
           => OrderTRep t o f (Element o)
newMaximum = fromRepNew ElementRep.newMaximum

newAfter :: (StateMonadTrans t, Applicative f)
         => Element o
         -> OrderTRep t o f (Element o)
newAfter (Element elemRep) = fromRepNewNeighbor ElementRep.newAfter elemRep

newBefore :: (StateMonadTrans t, Applicative f)
          => Element o
          -> OrderTRep t o f (Element o)
newBefore (Element elemRep) = fromRepNewNeighbor ElementRep.newBefore elemRep

fromRepNewNeighbor :: (StateMonadTrans t, Applicative f)
                   => (forall o' e' . ElementRep o' e' ->
                                      OrderRep o' e'   ->
                                      IO (ElementRep o' e'))
                   -> ElementRep o'' e''
                   -> OrderTRep t o f (Element o)
fromRepNewNeighbor repNewNeighbor elemRep = orderTRep where

    orderTRep = fromRepNew (repNewNeighbor (unsafeCoerce elemRep))

fromRepNew :: (StateMonadTrans t, Applicative f)
           => (forall o' e' . OrderRep o' e' -> IO (ElementRep o' e'))
           -> OrderTRep t o f (Element o)
fromRepNew repNew = OrderTRep $ state fun where

    fun orderRep = (elem, elem `seq` orderRep) where

        {-# NOINLINE elem #-}
        elem = unsafePerformIO $ Element <$> repNew orderRep

-- * State monad transformers

state :: (StateMonadTrans t, Applicative f) => (s -> (a, s)) -> t s f a
state fun = stateT $ pure . fun

{-NOTE:
    We cannot write constraints like (forall m . Monad m => Monad t s m) to
    constrain t. So we introduce the class StateMonadTrans, which has, for
    example, monad operations for t s m with a constraint Monad m, witnessing
    that t s m is a monad if m is.
-}
class StateMonadTrans t where

    -- Construction and running

    stateT :: (s -> f (a, s)) -> t s f a

    runStateT :: t s f a -> s -> f (a, s)

    -- Functor

    fmap' :: Functor f => (a -> b) -> t s f a -> t s f b

    (<$!) :: Functor f => b -> t s f a -> t s f b

    -- Applicative

    pure' :: Monad m => a -> t s m a

    (<*>!) :: Monad m => t s m (a -> b) -> t s m a -> t s m b

    (*>!) :: Monad m => t s m a -> t s m b -> t s m b

    (<*!) :: Monad m => t s m a -> t s m b -> t s m a

    -- Alternative

    empty' :: MonadPlus m => t s m a

    (<|>!) :: MonadPlus m => t s m a -> t s m a -> t s m a

    some' :: MonadPlus m => t s m a -> t s m [a]

    many' :: MonadPlus m => t s m a -> t s m [a]

    -- Monad

    (>>=!) :: Monad m => t s m a -> (a -> t s m b) -> t s m b

    (>>!) :: Monad m => t s m a -> t s m b -> t s m b

    return' :: Monad m => a -> t s m a

    fail' :: Monad m => String -> t s m a

    -- MonadPlus

    mzero' :: MonadPlus m => t s m a

    mplus' :: MonadPlus m => t s m a -> t s m a -> t s m a

    -- MonadFix

    mfix' :: MonadFix m => (a -> t s m a) -> t s m a

    -- MonadTrans

    lift' :: Monad m => m a -> t s m a

    -- MonadIO

    liftIO' :: MonadIO m => IO a -> t s m a
