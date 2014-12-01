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
-}

-- * General things

-- NOTE: Algorithm is imported from the Data.OrderMaintenance.Algorithm.Type.

defaultAlgorithm :: Algorithm
defaultAlgorithm = dumb

-- * Specific algorithms

dumb :: Algorithm
dumb = Dumb.algorithm
