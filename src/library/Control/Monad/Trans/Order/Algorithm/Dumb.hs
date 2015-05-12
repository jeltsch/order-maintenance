module Control.Monad.Trans.Order.Algorithm.Dumb (

    algorithm

) where

-- Control

import Control.Applicative
import Control.Monad.ST
import Control.Monad.Trans.Order.Algorithm.Type
import Control.Monad.Trans.Order.Raw

-- Data

import           Data.Ratio
import           Data.STRef
import qualified Data.Set as Set
import           Data.Set (Set)

algorithm :: Algorithm
algorithm = Algorithm rawAlgorithm

data Dumb

type instance OrderCell Dumb s = PureOrder

type instance ElementCell Dumb s = PureElement

type PureOrder = Set PureElement

type PureElement = Rational

rawAlgorithm :: RawAlgorithm Dumb s
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

fromPure :: (PureOrder -> (a, PureOrder)) -> RawOrder Dumb s -> ST s a
fromPure trans rawOrder = do
                              pureOrder <- readSTRef rawOrder
                              let (output, pureOrder') = trans pureOrder
                              writeSTRef rawOrder pureOrder'
                              return output

fromPureInsert :: (PureOrder -> PureElement)
               -> RawOrder Dumb s
               -> ST s (RawElement Dumb s)
fromPureInsert trans rawOrder = fromPure trans' rawOrder >>= newSTRef where

    trans' pureOrder = let

                           pureElement = trans pureOrder

                       in (pureElement, Set.insert pureElement pureOrder)

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
