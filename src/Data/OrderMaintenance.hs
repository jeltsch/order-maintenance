module Data.OrderMaintenance (

    -- * Order computations
    OrderComp,
    compose,
    finish,
    branch,
    evalOrderComp,

    -- * Elements
    Element,
    withNewMinimum,
    withNewMaximum,
    withNewAfter,
    withNewBefore

) where

-- Control

import Control.Monad.Trans.Cont
import Control.Monad.ST
import Control.Concurrent.MVar

-- Data

import Data.OrderMaintenance.Raw

-- System

import System.IO.Unsafe

-- FIXME: We need to implement automatic deletion of elements.

-- * Order computations

newtype OrderComp o a = OrderComp (Order -> a)
-- FIXME: Implement OrderCompT, with an inner monad (as Order -> m a).
{-FIXME:
    Implement lazy and strict variants of OrderComp (should probably be
    lazy/strict in the order, that is, the state).
-}

data Order = Order (RawOrder RealWorld) Lock
-- NOTE: Evaluation of the Order constructor triggers the I/O for insertions.

compose :: ((forall a . OrderComp o a -> a) -> b) -> OrderComp o b
compose build = OrderComp $ \ order -> build (\ (OrderComp gen) -> gen order)

finish :: a -> OrderComp o a
finish val = compose (\ _ -> val)

branch :: OrderComp o a -> OrderComp o b -> OrderComp o (a,b)
branch comp1 comp2 = compose (\ eval -> (eval comp1,eval comp2))

{-NOTE:
    It is not possible to implement branches via Applicative. Applicative would
    allow the two results to be combined. This would allow us to combine
    elements from different branches with compare.

    We cannot have any form of sequential computation for OrderComp. If we had
    one, we could append a computation to a bundle of branches, thus comparing
    elements of different branches.

    The branching facility gives us some form of persistence of the order.
    Internally, however, this persistence is faked. All branches use the same
    data structure; this just cannot be really noticed by the user. There is
    also no performance penalty. The actual underlying order is larger than
    expected, but time complexity is O(1) and thus not dependent on the size of
    the order.

    Since the time complexity for an insertion is O(1), the additional space
    cost is also O(1). So if we ignore constant factors, then it also does not
    matter regarding space that several branches (actually all order
    computations) use the same underlying data structure. If we care about
    precise space usage, it does matter however. Let us compare our situation to
    sets. Say we have a set s. The additional space usage of adding k elements
    to s is always the same, no matter what elements we might add to s in
    another branch. In the case of orders, this is not necessarily the case. A
    similar argument holds for time complexity.
-}

evalOrderComp :: (forall o . OrderComp o a) -> a
evalOrderComp (OrderComp gen) = gen emptyOrder
-- FIXME: This and emptyOrder should be parameterized by the algorithm.

emptyOrder :: Order
emptyOrder = unsafePerformIO $ do
    rawOrder <- stToIO newOrder
    lock <- newLock
    return (Order rawOrder lock)
{-FIXME:
    Introduce the safety measures for unsafePerformIO. It should not matter how
    many times the I/O is performed.
-}

-- * Elements

data Element o = Element (RawElement RealWorld) Lock
-- NOTE: Evaluation of the Element constructor triggers the I/O for insertions.

instance Eq (Element o) where

    Element rawElem1 _ == Element rawElem2 _ = rawElem1 == rawElem2
{-FIXME:
    This assumes that raw elements are references. At the moment, this cannot be
    generally assumed.
-}

instance Ord (Element o) where

    compare (Element rawElem1 lock) (Element rawElem2 _) = ordering where
    
        ordering = unsafePerformIO $
                   criticalSection lock $
                   stToIO $ compareElements rawElem1 rawElem2
{-FIXME:
    Introduce the safety measures for unsafePerformIO. It should not matter how
    many times the I/O is performed.
-}

fromInsert :: (RawOrder RealWorld -> ST RealWorld (RawElement RealWorld))
           -> (Element o -> OrderComp o a) -> OrderComp o a
fromInsert insert cont = OrderComp gen where

    gen order = let

                    (elem,order') = explicitStateInsert order

                    OrderComp contGen = cont elem

                in contGen order'

    explicitStateInsert order@(Order rawOrder lock) = unsafePerformIO $
        criticalSection lock $
        do
            rawElem <- stToIO $ insert rawOrder
            return (Element rawElem lock,order)
{-FIXME:
    Introduce the safety measures for unsafePerformIO. The I/O must occur only
    once.
-}

withNewMinimum :: (Element o -> OrderComp o a) -> OrderComp o a
withNewMinimum = fromInsert insertMinimum

withNewMaximum :: (Element o -> OrderComp o a) -> OrderComp o a
withNewMaximum = fromInsert insertMaximum

withNewAfter :: Element o -> (Element o -> OrderComp o a) -> OrderComp o a
withNewAfter (~(Element rawElem _)) = fromInsert (insertAfter rawElem)

withNewBefore :: Element o -> (Element o -> OrderComp o a) -> OrderComp o a
withNewBefore (~(Element rawElem _)) = fromInsert (insertBefore rawElem)

{-FIXME:
    The actual implementation has explicit deletions and uses the ST monad. It
    is not exported (mainly because of the explicit deletions).
    
    The internal implementation universally quantifies the o-parameter to make
    sure that it does not use any actual I/O. However, we later turn the ST
    computation into an IO computation using RealWorld. STRefs that internally
    serve as elements become IORefs.

    Should we use lazy or strict ST? Since we turn the ST computation into an IO
    computation, it seems sensible to use strict ST. If we need to form a fixed
    point, however, it might be necessary to form this fixed point in lazy ST.
-}

{-FIXME:
    Implement the Trans type based on any t which has a run operation of type
    forall s . t s a -> a and which has the property that for every s, t s is a
    monad. This then includes ST and OT.

    For incremental sorting, use a type T defined by

        T s = StateT AdditionalState (OT s)  .

    Hopefully, we can derive a run operation for T from runOT.
-}

-- * Locks

type Lock = MVar ()

newLock :: IO Lock
newLock = newEmptyMVar

criticalSection :: Lock -> IO a -> IO a
criticalSection lock act = do
    putMVar lock ()
    val <- act
    takeMVar lock
    return val
