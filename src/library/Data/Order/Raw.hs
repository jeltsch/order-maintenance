module Data.Order.Raw (

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

type RawOrder a s = STRef s (OrderCell a s)

type family OrderCell a s

type RawElement a s = STRef s (ElementCell a s)

type family ElementCell a s

data RawAlgorithm a s = RawAlgorithm {
    newOrder        :: ST s (RawOrder a s),
    compareElements :: RawOrder a s
                    -> RawElement a s
                    -> RawElement a s
                    -> ST s Ordering,
    newMinimum      :: RawOrder a s -> ST s (RawElement a s),
    newMaximum      :: RawOrder a s -> ST s (RawElement a s),
    newAfter        :: RawOrder a s -> RawElement a s -> ST s (RawElement a s),
    newBefore       :: RawOrder a s -> RawElement a s -> ST s (RawElement a s),
    delete          :: RawOrder a s -> RawElement a s -> ST s ()
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
