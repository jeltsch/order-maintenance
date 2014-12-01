module Data.OrderMaintenance.Raw (

    RawOrder,
    RawElement,
    ElementCell,
    RawAlgorithm (
        RawAlgorithm,
        newOrder,
        compareElements,
        insertMinimum,
        insertMaximum,
        insertAfter,
        insertBefore,
        delete
    )

) where

import Control.Monad.ST
import Data.STRef

{-FIXME:
    Implement the following:

      • a dumb algorithm that uses sets of real numbers, which are represented
        as bit strings

      • a not so dump algorithm that uses arbitarily deep log-trees

      • the file maintenance algorithm by Bender et al. combined with log-trees
        of fixed height

      • a function that converts any algorithm into one that shifts elements
        between two orders upon deletion (for avoiding sparsly populated order
        structures)
-}

type family RawOrder o :: * -> *

type RawElement o s = STRef s (ElementCell o s)

type family ElementCell o :: * -> *

data RawAlgorithm o s = RawAlgorithm {
    newOrder        :: ST s (RawOrder o s),
    compareElements :: RawElement o s -> RawElement o s -> ST s Ordering,
    insertMinimum   :: RawOrder o s -> ST s (RawElement o s),
    insertMaximum   :: RawOrder o s -> ST s (RawElement o s),
    insertAfter     :: RawElement o s -> RawOrder o s -> ST s (RawElement o s),
    insertBefore    :: RawElement o s -> RawOrder o s -> ST s (RawElement o s),
    delete          :: RawElement o s -> RawOrder o s -> ST s ()
}
