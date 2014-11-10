module Data.OrderMaintenance (

    -- * Order computations
    OrderComp,
    finish,
    branch,
    runOrderComp,

    -- * Elements
    Element,
    withNewMinimum,
    withNewMaximum,
    withNewAfter,
    withNewBefore

) where

import Control.Applicative
import Control.Monad.Trans.Cont

-- * Order computations

data OrderComp o a = OrderComp
-- FIXME: Imlementation to be decided.

finish :: a -> OrderComp o a
finish = undefined

branch :: OrderComp o a -> OrderComp o b -> OrderComp o (a,b)
branch = undefined

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

runOrderComp :: (forall o . OrderComp o a) -> a
runOrderComp = undefined

-- * Elements

data Element o = Element
-- FIXME: Imlementation to be decided.

instance Eq (Element o) where

    elem1 == elem2 = compare elem1 elem2 == EQ
-- FIXME: Maybe, Eq can just be derived.

instance Ord (Element o) where

    compare = undefined

withNewMinimum :: (Element o -> OrderComp o a) -> OrderComp o a
withNewMinimum = undefined

withNewMaximum :: (Element o -> OrderComp o a) -> OrderComp o a
withNewMaximum = undefined

withNewAfter :: Element o -> (Element o -> OrderComp o a) -> OrderComp o a
withNewAfter = undefined

withNewBefore :: Element o -> (Element o -> OrderComp o a) -> OrderComp o a
withNewBefore = undefined

{-FIXME:
    The actual implementation has explicit deletions and uses the ST monad. It
    is not exported (mainly because of the explicit deletions).
    
    The internal implementation universally quantifies the o-parameter to make
    sure that it does not use any actual I/O. However, we later turn the ST
    computation into an IO computation using RealWorld. STRefs that internally
    serve as elements become IORefs.

    The ST computation receives a stream of commands that include ST-based
    continuations for returning results. When we finally instantiate o to
    RealWorld, these continuations actually become IO-based and can just store
    the result in an MVar. The outer shell of the order maintenance module
    creates these MVars and fetches the results from them.

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
