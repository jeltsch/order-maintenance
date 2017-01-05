module Data.Order.Element.IO.Type (

    Element (Element)

) where

-- Data

import           Data.Order.Element.Representation
import qualified Data.Order.Algorithm.Raw.Default as Default

newtype Element = Element (ElementRep Default.OrderCell Default.ElementCell)
                  deriving (Eq, Ord)
