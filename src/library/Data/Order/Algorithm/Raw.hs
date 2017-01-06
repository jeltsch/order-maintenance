module Data.Order.Algorithm.Raw (

    RawOrder,
    RawElement,
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

-- Control

import Control.Monad.ST

-- Data

import Data.STRef

type RawOrder s o = STRef s (o s)

type RawElement s e = STRef s (e s)

data RawAlgorithm s o e = RawAlgorithm {
    newOrder        :: ST s (RawOrder s o),
    compareElements :: RawElement s e -> RawElement s e -> RawOrder s o -> ST s Ordering,
    newMinimum      :: RawOrder s o -> ST s (RawElement s e),
    newMaximum      :: RawOrder s o -> ST s (RawElement s e),
    newAfter        :: RawElement s e -> RawOrder s o -> ST s (RawElement s e),
    newBefore       :: RawElement s e -> RawOrder s o -> ST s (RawElement s e),
    delete          :: RawElement s e -> RawOrder s o -> ST s ()
}
{-FIXME:
    If we ever allow users to plug in their own algorithms, we have to flag the
    respective function as unsafe and point out that referential transparency is
    in danger if the algorithm does not fulfill the specification.
-}
