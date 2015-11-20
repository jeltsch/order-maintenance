module Data.Order.Algorithm.Type (

    Algorithm (Algorithm)

) where

import Data.Order.Raw

data Algorithm = forall a . Algorithm (forall s . RawAlgorithm a s)
