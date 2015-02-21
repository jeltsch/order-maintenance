module Control.Monad.Trans.Order.Lazy.Internals (

    -- * The lazy OrderT monad transformer

    OrderT (OrderT),
    OrderRep (OrderRep),
    emptyOrderRep,

    -- * Locks

    Lock,
    criticalSection

) where

-- Control

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.ST
import Control.Concurrent.MVar
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

data OrderRep o = OrderRep (RawOrder o RealWorld)
                           (RawAlgorithm o RealWorld)
                           Lock
-- FIXME: Maybe use OrderedSet instead of OrderRep.
-- NOTE: Evaluation of the OrderRep constructor triggers the I/O for insertions.

emptyOrderRep :: (forall s . RawAlgorithm o s) -> OrderRep o
emptyOrderRep rawAlg = unsafePerformIO $ do
    rawOrder <- stToIO (newOrder rawAlg)
    lock <- newLock
    return (OrderRep rawOrder rawAlg lock)
{-FIXME:
    Introduce the safety measures for unsafePerformIO. It should not matter
    how many times the I/O is performed.
-}

-- * Locks

type Lock = MVar ()

newLock :: IO Lock
newLock = newEmptyMVar

criticalSection :: Lock -> IO a -> IO a
criticalSection lock act = do
    putMVar lock ()
    val <- act
    takeMVar lock
    return val
