module Control.Monad.Trans.Order.Lazy.Internals (

    -- * The lazy OrderT monad transformer

    OrderT (OrderT),
    OrderRep (OrderRep),
    emptyOrderRep,

    -- * Gates

    Gate,
    withRawOrder

) where

-- Control

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.ST
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Trans.Order.Raw

-- System

import System.IO.Unsafe

-- * The lazy OrderT monad transformer

newtype OrderT o m a = OrderT (StateT (OrderRep o) m a) deriving (
    Functor,
    Applicative,
    Alternative,
    Monad,
    MonadPlus,
    MonadTrans,
    MonadIO)
    -- FIXME: Should we also have a MonadFix instance?

data OrderRep o = OrderRep (RawAlgorithm o RealWorld) (Gate o)
-- FIXME: Maybe use OrderedSet instead of OrderRep.
-- NOTE: Evaluation of the OrderRep constructor triggers the I/O for insertions.

emptyOrderRep :: (forall s . RawAlgorithm o s) -> OrderRep o
emptyOrderRep rawAlg = unsafePerformIO $ do
    rawOrder <- stToIO (newOrder rawAlg)
    gate <- newGate rawOrder
    return (OrderRep rawAlg gate)
{-FIXME:
    Introduce the safety measures for unsafePerformIO. It should not matter
    how many times the I/O is performed.
-}

-- * Gates

newtype Gate a = Gate (MVar (RawOrder a RealWorld))

newGate :: RawOrder a RealWorld -> IO (Gate a)
newGate = fmap Gate . newMVar

withRawOrder :: Gate a -> (RawOrder a RealWorld -> IO r) -> IO r
withRawOrder (Gate mVar) cont = bracket (takeMVar mVar) (putMVar mVar) cont
