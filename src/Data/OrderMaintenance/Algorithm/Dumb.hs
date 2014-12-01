module Data.OrderMaintenance.Algorithm.Dumb (

    dumbAlgorithm

) where

import Data.OrderMaintenance.Algorithm.Type
import Data.OrderMaintenance.Raw

dumbAlgorithm :: Algorithm
dumbAlgorithm = Algorithm rawDumbAlgorithm

rawDumbAlgorithm :: RawAlgorithm o s
rawDumbAlgorithm = undefined
