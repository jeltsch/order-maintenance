module Data.Order.Pair.Type (

    OrderPair (OrderPair)

) where

-- Data

import Data.Order.Representation

data OrderPair o a = forall o' e' . OrderPair (a, OrderRep o' e')
