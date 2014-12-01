module Data.OrderMaintenance.Raw (

    RawOrder,
    RawElement,
    RawAlgorithm (
        RawAlgorithm,
        newOrder,
        compareElements,
        insertMinimum,
        insertMaximum,
        insertAfter,
        insertBefore
    )

) where

import Control.Monad.ST

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

type family RawElement o :: * -> *

data RawAlgorithm o s = Eq (RawElement o s) => RawAlgorithm {
    newOrder        :: ST s (RawOrder o s),
    compareElements :: RawElement o s -> RawElement o s -> ST s Ordering,
    insertMinimum   :: RawOrder o s -> ST s (RawElement o s),
    insertMaximum   :: RawOrder o s -> ST s (RawElement o s),
    insertAfter     :: RawElement o s -> RawOrder o s -> ST s (RawElement o s),
    insertBefore    :: RawElement o s -> RawOrder o s -> ST s (RawElement o s)
}
