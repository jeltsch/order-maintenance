module Data.Order.Raw.Algorithm.Dumb (

    Algorithm,
    rawAlgorithm

) where

-- Control

import Control.Applicative
import Control.Monad.ST

-- Data

import           Data.Ratio
import           Data.STRef
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Order.Raw

data Algorithm

type instance OrderCell Algorithm s = PureOrder

type instance ElementCell Algorithm s = PureElement

type PureOrder = Set PureElement

type PureElement = Rational

rawAlgorithm :: RawAlgorithm Algorithm s
rawAlgorithm = RawAlgorithm {
    newOrder        = newSTRef Set.empty,
    compareElements = \ rawElem1 rawElem2 _ -> do
                          pureElem1 <- readSTRef rawElem1
                          pureElem2 <- readSTRef rawElem2
                          return (compare pureElem1 pureElem2),
    newMinimum      = fromPureInsert pureInsertMinimum,
    newMaximum      = fromPureInsert pureInsertMaximum,
    newAfter        = relative fromPureInsert pureInsertAfter,
    newBefore       = relative fromPureInsert pureInsertBefore,
    delete          = relative fromPure pureDelete
}

fromPure :: (PureOrder -> (a, PureOrder)) -> RawOrder Algorithm s -> ST s a
fromPure trans rawOrder = do
                              pureOrder <- readSTRef rawOrder
                              let (output, pureOrder') = trans pureOrder
                              writeSTRef rawOrder pureOrder'
                              return output

fromPureInsert :: (PureOrder -> PureElement)
               -> RawOrder Algorithm s
               -> ST s (RawElement Algorithm s)
fromPureInsert trans rawOrder = fromPure trans' rawOrder >>= newSTRef where

    trans' pureOrder = let

                           pureElement = trans pureOrder

                       in (pureElement, Set.insert pureElement pureOrder)

relative :: ((PureOrder -> a) -> RawOrder Algorithm s -> ST s b)
         -> (PureElement -> PureOrder -> a)
         -> RawElement Algorithm s
         -> RawOrder Algorithm s
         -> ST s b
relative conv trans rawElem rawOrder = do
    pureElem <- readSTRef rawElem
    conv (trans pureElem) rawOrder

pureInsertMinimum :: PureOrder -> PureElement
pureInsertMinimum pureOrder
    | Set.null pureOrder = 1 % 2
    | otherwise          = Set.findMin pureOrder / 2

pureInsertMaximum :: PureOrder -> PureElement
pureInsertMaximum pureOrder
    | Set.null pureOrder = 1 % 2
    | otherwise          = (Set.findMax pureOrder + 1) / 2

pureInsertAfter :: PureElement -> PureOrder -> PureElement
pureInsertAfter pureElement pureOrder = pureElement' where

    greater = snd (Set.split pureElement pureOrder)

    pureElement' | Set.null greater = (pureElement + 1) / 2
                 | otherwise        = (pureElement + Set.findMin greater) / 2

pureInsertBefore :: PureElement -> PureOrder -> PureElement
pureInsertBefore pureElement pureOrder = pureElement' where

    lesser = fst (Set.split pureElement pureOrder)

    pureElement' | Set.null lesser = pureElement / 2
                 | otherwise       = (pureElement + Set.findMax lesser) / 2

pureDelete :: PureElement -> PureOrder -> ((), PureOrder)
pureDelete pureElement pureOrder = ((), Set.delete pureElement pureOrder)
