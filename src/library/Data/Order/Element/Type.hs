module Data.Order.Element.Type (

    Element (Element)

) where

-- Data

import Data.Order.Element.Representation

-- Unsafe

import Unsafe.Coerce

data Element o = forall o e . Element !(ElementRep o e)
{-NOTE:
    When using OrderT, reduction of an Element value to WHNF triggers the I/O
    for insertions.
-}

instance Eq (Element o) where

    Element elemRep1 == Element elemRep2 = elemRep1 == unsafeCoerce elemRep2

instance Ord (Element o) where

    compare (Element elemRep1) (Element elemRep2) = ordering where

        ordering = compare elemRep1 (unsafeCoerce elemRep2)
