module Data.OrderMaintenance.Algorithm (

    -- * General things
    Algorithm,
    defaultAlgorithm,

    -- * Specific algorithms
    dumbAlgorithm

) where

import Data.OrderMaintenance.Algorithm.Type
import Data.OrderMaintenance.Algorithm.Dumb

defaultAlgorithm :: Algorithm
defaultAlgorithm = dumbAlgorithm
