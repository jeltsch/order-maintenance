module Control.Monad.Trans.Order.Raw (

    RawOrder,
    OrderCell,
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

type RawOrder o s = STRef s (OrderCell o s)

type family OrderCell o s

type RawElement o s = STRef s (ElementCell o s)

type family ElementCell o s

data RawAlgorithm o s = RawAlgorithm {
    newOrder        :: ST s (RawOrder o s),
    compareElements :: RawElement o s -> RawElement o s -> ST s Ordering,
    insertMinimum   :: RawOrder o s -> ST s (RawElement o s),
    insertMaximum   :: RawOrder o s -> ST s (RawElement o s),
    insertAfter     :: RawElement o s -> RawOrder o s -> ST s (RawElement o s),
    insertBefore    :: RawElement o s -> RawOrder o s -> ST s (RawElement o s),
    delete          :: RawElement o s -> RawOrder o s -> ST s ()
}
