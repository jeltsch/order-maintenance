module Data.Order.Algorithm.Raw.Default (

    OrderCell,
    ElementCell,
    rawAlgorithm

) where

-- Data

import           Data.Order.Algorithm.Raw
import qualified Data.Order.Algorithm.Raw.DietzSleatorAmortizedLog
                     as DietzSleatorAmortizedLog

type OrderCell = DietzSleatorAmortizedLog.OrderCell

type ElementCell = DietzSleatorAmortizedLog.ElementCell

rawAlgorithm :: RawAlgorithm s OrderCell ElementCell
rawAlgorithm = DietzSleatorAmortizedLog.rawAlgorithm
