module Data.OrderMaintenance.Algorithm (

    -- * General things
    Algorithm,
    defaultAlgorithm,

    -- * Specific algorithms
    dumb

) where

import Data.OrderMaintenance.Algorithm.Type
import Data.OrderMaintenance.Algorithm.Dumb as Dumb

-- * General things

-- NOTE: Algorithm is imported from the Data.OrderMaintenance.Algorithm.Type.

defaultAlgorithm :: Algorithm
defaultAlgorithm = dumb

-- * Specific algorithms

dumb :: Algorithm
dumb = Dumb.algorithm
