module Data.OrderMaintenance.Algorithm (

    -- * General things

    Algorithm,
    defaultAlgorithm,

    -- * Specific algorithms

    dumb

) where

import Data.OrderMaintenance.Algorithm.Type
import Data.OrderMaintenance.Algorithm.Dumb as Dumb

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

-- * General things

-- NOTE: Algorithm is imported from Data.OrderMaintenance.Algorithm.Type.

defaultAlgorithm :: Algorithm
defaultAlgorithm = dumb

-- * Specific algorithms

dumb :: Algorithm
dumb = Dumb.algorithm
