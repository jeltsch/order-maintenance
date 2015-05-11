module Control.Monad.Trans.Order.Raw (

    RawOrder,
    OrderCell,
    RawElement,
    ElementCell,
    RawAlgorithm (
        RawAlgorithm,
        newOrder,
        compareElements,
        newMinimum,
        newMaximum,
        newAfter,
        newBefore,
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
    newMinimum      :: RawOrder o s -> ST s (RawElement o s),
    newMaximum      :: RawOrder o s -> ST s (RawElement o s),
    newAfter        :: RawElement o s -> RawOrder o s -> ST s (RawElement o s),
    newBefore       :: RawElement o s -> RawOrder o s -> ST s (RawElement o s),
    delete          :: RawElement o s -> RawOrder o s -> ST s ()
}
{-FIXME:
    If we ever allow users to plug in their own algorithms, we have to flag the
    respective function as unsafe and point out that referential transparency is
    in danger if the algorithm does not fulfill the specification. This is
    because element comparison is presented to the user as a pure function. The
    important condition is that for any two elements, compareElements must
    always return the same result as long as delete is not called on either
    element.
-}
