module Data.OrderMaintenance.Raw (

    -- * Orders
    RawOrder,
    newOrder,

    -- * Elements
    RawElement,
    compareElements,
    insertMinimum,
    insertMaximum,
    insertAfter,
    insertBefore

) where

-- Control
import Control.Monad.ST

-- Data
import Data.STRef

{-
!!! IMPORTANT:
At least for comparison, define a type of order maintenance algorithms, bundling the operations; then have a value of this type for the hacked solution, one for the real thing™, and one endofuntion on algorithms that adds this moving of elements when elements are deleted. Then parameterize runOrderComp by the algorithm. The real thing™ algorithm would then be parameterized by the speed.

??? We should turn OT into some sort of monad transformer. For this to work, the insert operations of OrderComp probably have to work with monads as well (CPS with a monad). Would they be safe with any monad? Are there any insertion operations that are more core and from which the monadic ones could be derived?
-}

-- * Orders

type RawOrder s = STRef s OrderCell

data OrderCell = OrderCell -- to be implemented

newOrder :: ST s (RawOrder s)
newOrder = undefined

-- * Elements

type RawElement s = STRef s ElementCell

data ElementCell = ElementCell -- to be implemented

compareElements :: RawElement s -> RawElement s -> ST s Ordering
compareElements = undefined

insertMinimum :: RawOrder s -> ST s (RawElement s)
insertMinimum = undefined

insertMaximum :: RawOrder s -> ST s (RawElement s)
insertMaximum = undefined

insertAfter :: RawElement s -> RawOrder s -> ST s (RawElement s)
insertAfter = undefined

insertBefore :: RawElement s -> RawOrder s -> ST s (RawElement s)
insertBefore = undefined
