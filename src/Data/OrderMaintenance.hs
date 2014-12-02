module Data.OrderMaintenance (

    -- * Order computations

    OrderComp,
    evalOrderComp,
    evalOrderCompWith,
    composeOrderComp,

    -- * Order computations with an inner monad

    OrderCompT,
    evalOrderCompT,
    evalOrderCompTWith,
    composeOrderCompT,

    -- * Specific order computation compositions

    finish,
    branch,
    withOutputOf,
    withForcedOrder,

    -- * Elements

    Element,
    withNewMinimum,
    withNewMaximum,
    withNewAfter,
    withNewBefore

) where

-- Control

import Control.Applicative
import Control.Monad.ST
import Control.Concurrent.MVar

-- Data

import Data.Monoid
import Data.Functor.Identity
import Data.IORef
import Data.OrderMaintenance.Raw
import Data.OrderMaintenance.Algorithm
import Data.OrderMaintenance.Algorithm.Type

-- System

import System.IO.Unsafe

-- GHC
import GHC.IORef -- for converting from STRef RealWorld to IORef

{-FIXME:
    We should declare this module as trustworthy, but for this, we might have to
    forbid outsiders to construct their own algorithms.
-}

-- * Order computations

type OrderComp o = OrderCompT o Identity

evalOrderComp :: (forall o . OrderComp o a) -> a
evalOrderComp comp = runIdentity (evalOrderCompT comp)

evalOrderCompWith :: Algorithm -> (forall o . OrderComp o a) -> a
evalOrderCompWith alg comp = runIdentity (evalOrderCompTWith alg comp)

composeOrderComp :: ((forall a . OrderComp o a -> a) -> b) -> OrderComp o b
composeOrderComp build = composeOrderCompT $
                         \ eval' -> Identity $ build (runIdentity . eval')

-- * Order computations with an inner monad

newtype OrderCompT o m a = OrderCompT (Order o -> m a)

instance Alternative m => Monoid (OrderCompT o m a) where

    mempty = composeOrderCompT $
             \ _ -> empty

    comp1 `mappend` comp2 = composeOrderCompT $
                            \ eval -> eval comp1 <|> eval comp2

data Order o = Order (RawOrder o RealWorld)
                     (RawAlgorithm o RealWorld)
                     Lock
-- NOTE: Evaluation of the Order constructor triggers the I/O for insertions.

evalOrderCompT :: (forall o . OrderCompT o m a) -> m a
evalOrderCompT = evalOrderCompTWith defaultAlgorithm

evalOrderCompTWith :: Algorithm -> (forall o . OrderCompT o m a) -> m a
evalOrderCompTWith (Algorithm rawAlg) (OrderCompT gen) = gen emptyOrder where

    emptyOrder = unsafePerformIO $ do
        rawOrder <- stToIO (newOrder rawAlg)
        lock <- newLock
        return (Order rawOrder rawAlg lock)
    {-FIXME:
        Introduce the safety measures for unsafePerformIO. It should not matter
        how many times the I/O is performed.
    -}

composeOrderCompT :: ((forall a . OrderCompT o m a -> m a) -> m b)
                  -> OrderCompT o m b
composeOrderCompT build = OrderCompT gen where

    gen order = build (\ (OrderCompT gen) -> gen order)

-- * Specific order computation compositions

finish :: Applicative m => a -> OrderCompT o m a
finish val = composeOrderCompT $
             \ _ -> pure val

branch :: Applicative m =>
          OrderCompT o m a -> OrderCompT o m b -> OrderCompT o m (a,b)
branch comp1 comp2 = composeOrderCompT $
                     \ eval -> liftA2 (,) (eval comp1) (eval comp2)

withOutputOf :: Monad m => m a -> (a -> OrderCompT o m b) -> OrderCompT o m b
withOutputOf monad cont = composeOrderCompT $
                          \ eval -> monad >>= eval . cont

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

withForcedOrder :: OrderCompT o m a -> OrderCompT o m a
withForcedOrder (OrderCompT gen) = OrderCompT (gen $!)

-- * Elements

data Element o = Element (RawElement o RealWorld)
                         (RawAlgorithm o RealWorld)
                         Lock
-- NOTE: Evaluation of the Element constructor triggers the I/O for insertions.

instance Eq (Element o) where

    (==) (Element rawElem1 (RawAlgorithm _ _ _ _ _ _ _) _)
         (Element rawElem2 _                            _) = equal where

        equal = rawElem1 == rawElem2

instance Ord (Element o) where

    compare (Element rawElem1 rawAlg lock)
            (Element rawElem2 _      _)    = ordering where

        ordering = unsafePerformIO $
                   criticalSection lock $
                   stToIO $ compareElements rawAlg rawElem1 rawElem2
{-FIXME:
    Introduce the safety measures for unsafePerformIO. It should not matter how
    many times the I/O is performed.
-}

fromInsert :: (RawAlgorithm o RealWorld
                   -> RawOrder o RealWorld
                   -> ST RealWorld (RawElement o RealWorld))
           -> (Element o -> OrderCompT o m a)
           -> OrderCompT o m a
fromInsert insert cont = OrderCompT gen where

    gen order = let

                    (elem,order') = explicitStateInsert order

                    OrderCompT contGen = cont elem

                in contGen order'

    explicitStateInsert order@(Order rawOrder rawAlg lock) = output where

        output = unsafePerformIO $
                 criticalSection lock $
                 do
                     rawElem <- stToIO $ insert rawAlg rawOrder
                     mkWeakIORef (IORef rawElem)
                                 (criticalSection lock $
                                  stToIO $
                                  delete rawAlg rawElem rawOrder)
                     return (Element rawElem rawAlg lock,order)
    {-FIXME:
        Introduce the safety measures for unsafePerformIO. The I/O must occur only
        once.
    -}

withNewMinimum :: (Element o -> OrderCompT o m a)
               -> OrderCompT o m a
withNewMinimum = fromInsert insertMinimum

withNewMaximum :: (Element o -> OrderCompT o m a)
               -> OrderCompT o m a
withNewMaximum = fromInsert insertMaximum

withNewAfter :: Element o
             -> (Element o -> OrderCompT o m a)
             -> OrderCompT o m a
withNewAfter (~(Element rawElem _ _)) = fromInsert (flip insertAfter rawElem)

withNewBefore :: Element o
              -> (Element o -> OrderCompT o m a)
              -> OrderCompT o m a
withNewBefore (~(Element rawElem _ _)) = fromInsert (flip insertBefore rawElem)

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
