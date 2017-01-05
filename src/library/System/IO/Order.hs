module System.IO.Order (

    newMinimum,
    newMaximum,
    newAfter,
    newBefore

) where

-- Data

import           Data.Order.Element.IO.Type
import           Data.Order.Representation
import qualified Data.Order.Element.Representation as ElementRep
import qualified Data.Order.Algorithm.Raw.Default as Default

-- System

import System.IO.Unsafe

newMinimum :: IO Element
newMinimum = Element <$> ElementRep.newMinimum orderRep

newMaximum :: IO Element
newMaximum = Element <$> ElementRep.newMaximum orderRep

newAfter :: Element -> IO Element
newAfter (Element elemRep) = Element <$> ElementRep.newAfter elemRep orderRep

newBefore :: Element -> IO Element
newBefore (Element elemRep) = Element <$> ElementRep.newBefore elemRep orderRep

{-# NOINLINE orderRep #-}
orderRep :: OrderRep Default.OrderCell Default.ElementCell
orderRep = unsafePerformIO $ newOrderRep Default.rawAlgorithm
