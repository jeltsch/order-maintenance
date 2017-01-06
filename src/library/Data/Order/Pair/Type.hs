module Data.Order.Pair.Type (

    OrderPair (OrderPair)

) where

-- Data

import Data.Order.Representation

data OrderPair o a = forall o' e' . OrderPair (a, OrderRep o' e')

instance Functor (OrderPair o) where

    fmap fun (OrderPair (val, orderRep)) = OrderPair (fun val, orderRep)

    val <$ OrderPair (_, orderRep) = OrderPair (val, orderRep)
