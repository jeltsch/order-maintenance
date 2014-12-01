module Data.OrderMaintenance.Algorithm.Type (

    Algorithm (Algorithm)

) where

import Data.OrderMaintenance.Raw

data Algorithm = forall o . Algorithm (forall s . RawAlgorithm o s)
