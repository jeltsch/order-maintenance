module Data.Order.Algorithm.Type (

    Algorithm (Algorithm)

) where

import Data.Order.Algorithm.Raw

data Algorithm = forall o e . Algorithm (forall s . RawAlgorithm s o e)
