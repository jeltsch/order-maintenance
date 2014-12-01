module Data.OrderMaintenance.Algorithm.Dumb (

    algorithm

) where

-- Control

import Control.Applicative
import Control.Monad.ST

-- Data

import           Data.Function
import           Data.Bits
import           Data.STRef
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.OrderMaintenance.Algorithm.Type
import           Data.OrderMaintenance.Raw

algorithm :: Algorithm
algorithm = Algorithm rawAlgorithm

data Dumb

type instance OrderCell Dumb s = PureOrder

type instance ElementCell Dumb s = PureElement

type PureOrder = Set PureElement

type PureElement = [Bit]

oneHalf :: PureElement
oneHalf = [True]

averageWithZero :: PureElement -> PureElement
averageWithZero pureElem = False : pureElem

averageWithOne :: PureElement -> PureElement
averageWithOne pureElem = True : pureElem

average :: PureElement -> PureElement -> PureElement
average pureElem1 pureElem2 = carry : sum where

    (carry,sum) = add pureElem1 pureElem2

add :: PureElement -> PureElement -> (Bit,PureElement)
add []             pureElem2      = (False,pureElem2)
add pureElem1      []             = (False,pureElem1)
add (bit1 : bits1) (bit2 : bits2) = (carry,sum) where

    (carry',sum') = add bits1 bits2

    carry = bit1 && bit2 || bit1 && carry' || bit2 && carry'

    sum = bit1 `xor` bit2 `xor` carry' : sum'

type Bit = Bool

rawAlgorithm :: RawAlgorithm Dumb s
rawAlgorithm = RawAlgorithm {
    newOrder        = newSTRef Set.empty,
    compareElements = liftA2 compare `on` readSTRef,
    insertMinimum   = fromPureInsert pureInsertMinimum,
    insertMaximum   = fromPureInsert pureInsertMaximum,
    insertAfter     = relative fromPureInsert pureInsertAfter,
    insertBefore    = relative fromPureInsert pureInsertBefore,
    delete          = relative fromPure pureDelete
}

fromPure :: (PureOrder -> (a,PureOrder)) -> RawOrder Dumb s -> ST s a
fromPure trans rawOrder = do
                              pureOrder <- readSTRef rawOrder
                              let (output,pureOrder') = trans pureOrder
                              writeSTRef rawOrder pureOrder'
                              return output

fromPureInsert :: (PureOrder -> PureElement)
               -> RawOrder Dumb s
               -> ST s (RawElement Dumb s)
fromPureInsert trans rawOrder = fromPure trans' rawOrder >>= newSTRef where

    trans' pureOrder = let

                           pureElement = trans pureOrder

                       in (pureElement,Set.insert pureElement pureOrder)

relative :: ((PureOrder -> a) -> RawOrder Dumb s -> ST s b)
         -> (PureElement -> PureOrder -> a)
         -> RawElement Dumb s
         -> RawOrder Dumb s
         -> ST s b
relative conv trans rawElem rawOrder = do
    pureElem <- readSTRef rawElem
    conv (trans pureElem) rawOrder

pureInsertMinimum :: PureOrder -> PureElement
pureInsertMinimum pureOrder
    | Set.null pureOrder = oneHalf
    | otherwise          = averageWithZero (Set.findMin pureOrder)

pureInsertMaximum :: PureOrder -> PureElement
pureInsertMaximum pureOrder
    | Set.null pureOrder = oneHalf
    | otherwise          = averageWithOne (Set.findMax pureOrder)

pureInsertAfter :: PureElement -> PureOrder -> PureElement
pureInsertAfter pureElement pureOrder = pureElement' where

    greater = snd (Set.split pureElement pureOrder)

    pureElement' | Set.null greater = averageWithOne pureElement
                 | otherwise        = average pureElement (Set.findMin greater)

pureInsertBefore :: PureElement -> PureOrder -> PureElement
pureInsertBefore pureElement pureOrder = pureElement' where

    lesser = fst (Set.split pureElement pureOrder)

    pureElement' | Set.null lesser = averageWithZero pureElement
                 | otherwise       = average pureElement (Set.findMax lesser)

pureDelete :: PureElement -> PureOrder -> ((),PureOrder)
pureDelete pureElement pureOrder = ((),Set.delete pureElement pureOrder)
