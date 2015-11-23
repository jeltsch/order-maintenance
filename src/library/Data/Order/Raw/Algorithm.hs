module Data.Order.Raw.Algorithm (

    type DefaultAlgorithm,
    defaultRawAlgorithm

) where

import Data.Order.Raw
import Data.Order.Raw.Algorithm.DietzSleatorAmortizedLog
       as DietzSleatorAmortizedLog

type DefaultAlgorithm = DietzSleatorAmortizedLog.Algorithm

defaultRawAlgorithm :: RawAlgorithm DefaultAlgorithm s
defaultRawAlgorithm = DietzSleatorAmortizedLog.rawAlgorithm
