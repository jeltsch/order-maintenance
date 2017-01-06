module Control.Monad.Trans.Order.Lazy.Type (

    OrderT (OrderT, runOrderT)

) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.State.Lazy as Lazy
import           Control.Monad.Trans.Order.Representation hiding (lift)

newtype OrderT o m a = OrderT {
            runOrderT :: OrderTRep Lazy.StateT o m a
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

instance StateMonadTrans Lazy.StateT where

    -- Construction and destruction

    stateT = Lazy.StateT

    runStateT = Lazy.runStateT

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

    lift' = lift

    -- MonadIO

    liftIO' = liftIO
