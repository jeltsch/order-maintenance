module Control.Monad.Trans.Order.Algorithm.Type (

    Algorithm (Algorithm)

) where

import Control.Monad.Trans.Order.Raw

data Algorithm = forall o . Algorithm (forall s . RawAlgorithm o s)
