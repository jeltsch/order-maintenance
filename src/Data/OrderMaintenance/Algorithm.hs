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

-- * General things

-- NOTE: Algorithm is imported from Data.OrderMaintenance.Algorithm.Type.

defaultAlgorithm :: Algorithm
defaultAlgorithm = dumb

-- * Specific algorithms

dumb :: Algorithm
dumb = Dumb.algorithm
