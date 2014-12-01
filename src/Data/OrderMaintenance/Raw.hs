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
