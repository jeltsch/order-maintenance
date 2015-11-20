module Control.Monad.Trans.Order.Lazy.Type (

    OrderT (OrderT)

) where

-- Control

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy

-- Data

import Data.Order.Internals

newtype OrderT o m a = OrderT (StateT (OrderRep o) m a) deriving (
    Functor,
    Applicative,
    Alternative,
    Monad,
    MonadPlus,
    MonadTrans,
    MonadIO)
    -- FIXME: Should we also have a MonadFix instance?
