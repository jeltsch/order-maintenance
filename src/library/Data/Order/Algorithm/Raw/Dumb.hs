module Data.Order.Algorithm.Raw.Dumb (

    OrderCell,
    ElementCell,
    rawAlgorithm

) where

-- Control

import Control.Applicative
import Control.Monad.ST

-- Data

import           Data.Order.Algorithm.Raw
import           Data.Ratio
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.STRef

newtype OrderCell s = OrderCell (Set Label)

newtype ElementCell s = ElementCell Label

type Label = Rational

rawAlgorithm :: RawAlgorithm s OrderCell ElementCell
rawAlgorithm = RawAlgorithm {
    newOrder        = newSTRef (OrderCell Set.empty),
    compareElements = \ rawElem1 rawElem2 _ -> do
                          ElementCell label1 <- readSTRef rawElem1
                          ElementCell label2 <- readSTRef rawElem2
                          return (compare label1 label2),
    newMinimum      = fromPureInsert pureInsertMinimum,
    newMaximum      = fromPureInsert pureInsertMaximum,
    newAfter        = relative fromPureInsert pureInsertAfter,
    newBefore       = relative fromPureInsert pureInsertBefore,
    delete          = relative fromPure pureDelete
}

fromPure :: (OrderCell s -> (a, OrderCell s))
         -> RawOrder s OrderCell
         -> ST s a
fromPure trans rawOrder = do
                              orderCell <- readSTRef rawOrder
                              let (output, orderCell') = trans orderCell
                              writeSTRef rawOrder orderCell'
                              return output

fromPureInsert :: (OrderCell s -> ElementCell s)
               -> RawOrder s OrderCell
               -> ST s (RawElement s ElementCell)
fromPureInsert trans rawOrder = fromPure trans' rawOrder >>= newSTRef where

    trans' orderCell@(OrderCell labels) = (elemCell, orderCell') where

        elemCell@(ElementCell label) = trans orderCell

        orderCell'= OrderCell (Set.insert label labels)

relative :: ((OrderCell s -> a) -> RawOrder s OrderCell -> ST s b)
         -> (ElementCell s -> OrderCell s -> a)
         -> RawElement s ElementCell
         -> RawOrder s OrderCell
         -> ST s b
relative conv trans rawElem rawOrder = do
    elemCell <- readSTRef rawElem
    conv (trans elemCell) rawOrder

pureInsertMinimum :: OrderCell s -> ElementCell s
pureInsertMinimum (OrderCell labels) = ElementCell label where

    label | Set.null labels = 1 % 2
          | otherwise       = Set.findMin labels / 2

pureInsertMaximum :: OrderCell s -> ElementCell s
pureInsertMaximum (OrderCell labels) = ElementCell label where

    label | Set.null labels = 1 % 2
          | otherwise       = (Set.findMax labels + 1) / 2

pureInsertAfter :: ElementCell s -> OrderCell s -> ElementCell s
pureInsertAfter (ElementCell label)
                (OrderCell labels)  = ElementCell label' where

    greater = snd (Set.split label labels)

    label' | Set.null greater = (label + 1) / 2
           | otherwise        = (label + Set.findMin greater) / 2

pureInsertBefore :: ElementCell s -> OrderCell s -> ElementCell s
pureInsertBefore (ElementCell label)
                 (OrderCell labels)  = ElementCell label' where


    lesser = fst (Set.split label labels)

    label' | Set.null lesser = label / 2
           | otherwise       = (label + Set.findMax lesser) / 2

pureDelete :: ElementCell s -> OrderCell s -> ((), OrderCell s)
pureDelete (ElementCell label)
           (OrderCell labels)  = ((), OrderCell labels') where

    labels' = Set.delete label labels
