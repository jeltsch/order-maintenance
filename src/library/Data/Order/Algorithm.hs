module Data.Order.Algorithm (

    -- * General things

    Algorithm,
    defaultAlgorithm,
    withRawAlgorithm,

    -- * Specific algorithms

    dumb,
    dietzSleatorAmortizedLog,
    dietzSleatorAmortizedLogWithSize

) where

-- Control

import Control.Monad.ST

-- Data

import           Data.Order.Algorithm.Type
import           Data.Order.Raw
import           Data.Order.Raw.Algorithm
import qualified Data.Order.Raw.Algorithm.Dumb
                 as Dumb
import qualified Data.Order.Raw.Algorithm.DietzSleatorAmortizedLog
                 as DietzSleatorAmortizedLog

{-FIXME:
    Implement the following:

      • an algorithm that uses arbitarily deep log-trees

      • the file maintenance algorithm by Bender et al. combined with log-trees
        of fixed height

      • a function that converts any algorithm into one that shifts elements
        between two orders upon deletion (for avoiding sparsly populated order
        structures)

    Maybe it makes sense to additionally offer the file maintenance algorithm by
    Bender et al. as an order maintenance algorithm in its own right.
-}

{-FIXME:
    For implementing Bender et al., it might be good to store the calibrator
    tree in an array, level by level from top to bottom. The array must then be
    created without initializing its elements. Initially the tree would be
    small; so few array elements would be used. When extending the tree, we
    would face the problem that initializing all the additionally used elements
    would take more than O(1) time. We can maybe use the trick by Barak A.
    Pearlmutter¹ (or a variant of it, specialized for our particular
    initialization pattern) to get O(1) time.

      ¹ See his e-mail to me from 5 December 2014.
-}

{-FIXME:
    More notes regarding implementing Bender et al.:

      • We can store the set of all children of a single node of a log-tree in
        an array of 48 64-bit words. Each word represents one child. Children
        are stored in the temporal order of their allocation. 48 bits of a word
        are the label, 3 are the left sibling index, 3 are the right sibling
        index. The parent pointer (pointer to the array plus index in the array)
        has to be stored only once per such an array, not for every child.

      • A block in the file maintenance data structure could encompass 48 or
        maybe also 64 elements. A 64-bit word could be used to store which of
        the array cells are taken by an element and which are free.

      • I think that on the upper two levels of a log tree, we need up to three
        times as many nodes for storing log-many subtrees, because of overflow
        nodes. This would mean that with the above approach, we could store up
        to 48 × 12 × 12 ≈ 7000 elements in a log tree and ca. 7000 × 48 ≈ 350000
        actual elements per file maintenance block. The total memory use would
        be a bit more than 8 × 350000 = 2.8 MB.

      • The number of actual elements per file maintenance block (350,000) would
        be a bit more than 2^18. Since our k would be 48, we could have up to
        2^48 × 2^18 = 2^66 elements theoretically. So we could reach the maximum
        of 2^64 elements.
-}

-- * General things

-- NOTE: Algorithm is imported from Data.OrderMaintenance.Algorithm.Type.

defaultAlgorithm :: Algorithm
defaultAlgorithm = Algorithm defaultRawAlgorithm

withRawAlgorithm :: Algorithm
                 -> (forall a . RawAlgorithm a s -> ST s r)
                 -> ST s r
withRawAlgorithm (Algorithm rawAlg) cont = cont rawAlg

-- * Specific algorithms

dumb :: Algorithm
dumb = Algorithm Dumb.rawAlgorithm

dietzSleatorAmortizedLog :: Algorithm
dietzSleatorAmortizedLog = Algorithm DietzSleatorAmortizedLog.rawAlgorithm

dietzSleatorAmortizedLogWithSize :: Int -> Algorithm
dietzSleatorAmortizedLogWithSize size
    = Algorithm (DietzSleatorAmortizedLog.rawAlgorithmWithSize size)
