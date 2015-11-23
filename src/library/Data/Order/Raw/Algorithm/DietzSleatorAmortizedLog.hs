module Data.Order.Raw.Algorithm.DietzSleatorAmortizedLog (

    Algorithm,
    rawAlgorithm,
    rawAlgorithmWithSize

) where

-- Control

import Control.Applicative
import Control.Monad
import Control.Monad.ST

-- Data

import Data.STRef
import Data.Word
import Data.Bits
import Data.Order.Raw

data Algorithm

type instance OrderCell Algorithm s = Cell s

type instance ElementCell Algorithm s = Cell s

data Cell s = Cell {
                  label :: Label,
                  next  :: CellRef s,
                  prev  :: CellRef s
              }

type CellRef s = STRef s (Cell s)

newtype Label = Label LabelWord deriving (Eq, Ord)

type LabelWord = Word64

labelWordSize :: Int
labelWordSize = 64

initialBaseLabel :: Label
initialBaseLabel = Label 0

rawAlgorithm :: RawAlgorithm Algorithm s
rawAlgorithm = rawAlgorithmWithSize defaultSize

defaultSize :: Int
defaultSize = 63

rawAlgorithmWithSize :: Int -> RawAlgorithm Algorithm s
rawAlgorithmWithSize size
    | size < 0 || size >= labelWordSize
        = error "Control.Monad.Trans.Order.Algorithm.DietzSleatorAmortizedLog: \
                \Size out of bounds"
    | otherwise
        = RawAlgorithm {
              newOrder        = fixST $
                                \ ref -> newSTRef $ Cell {
                                   label = initialBaseLabel,
                                   next  = ref,
                                   prev  = ref
                                },
              compareElements = \ baseRef ref1 ref2 -> do
                                    baseCell <- readSTRef baseRef
                                    cell1 <- readSTRef ref1
                                    cell2 <- readSTRef ref2
                                    let offset1 = labelDiff (label cell1)
                                                            (label baseCell)
                                    let offset2 = labelDiff (label cell2)
                                                            (label baseCell)
                                    return $ compare offset1 offset2,
              newMinimum      = newAfterCell,
              newMaximum      = newBeforeCell,
              newAfter        = const newAfterCell,
              newBefore       = const newBeforeCell,
              delete          = \ _ ref -> do
                                    cell <- readSTRef ref
                                    modifySTRef
                                        (prev cell)
                                        (\ prevCell -> prevCell {
                                                           next = next cell
                                                       })
                                    modifySTRef
                                        (next cell)
                                        (\ nextCell -> nextCell {
                                                           prev = prev cell
                                                       })
          } where

    noOfLabels :: LabelWord
    noOfLabels = shiftL 1 size

    labelMask :: LabelWord
    labelMask = pred noOfLabels

    toLabel :: LabelWord -> Label
    toLabel = Label . (.&. labelMask)

    labelSum :: Label -> Label -> Label
    labelSum (Label word1) (Label word2) = toLabel (word1 + word2)

    labelDiff :: Label -> Label -> Label
    labelDiff (Label word1) (Label word2) = toLabel (word1 - word2)

    labelDistance :: Label -> Label -> LabelWord
    labelDistance lbl1 lbl2 = case labelDiff lbl1 lbl2 of
                                  Label word | word == 0 -> noOfLabels
                                             | otherwise -> word

    newAfterCell :: CellRef s -> ST s (CellRef s)
    newAfterCell ref = do
        relabel ref
        lbl <- label <$> readSTRef ref
        nextRef <- next <$> readSTRef ref
        nextLbl <- label <$> readSTRef nextRef
        newRef <- newSTRef $ Cell {
            label = labelSum lbl (Label (labelDistance nextLbl lbl `div` 2)),
            next  = nextRef,
            prev  = ref
        }
        modifySTRef ref     (\ cell     -> cell     { next = newRef })
        modifySTRef nextRef (\ nextCell -> nextCell { prev = newRef })
        return newRef

    relabel :: CellRef s -> ST s ()
    relabel startRef = do
        startCell <- readSTRef startRef
        let delimSearch ref gapCount = do
                cell <- readSTRef ref
                let gapSum = labelDistance (label cell) (label startCell)
                if gapSum <= gapCount ^ 2
                    then if ref == startRef
                             then error "Control.Monad.Trans.Order.Algorithm.\
                                        \DietzSleatorAmortizedLog: \
                                        \Order full"
                             else delimSearch (next cell) (succ gapCount)
                    else return (ref, gapSum, gapCount)
        (delimRef, gapSum, gapCount) <- delimSearch (next startCell) 1
        let smallGap = gapSum `div` gapCount
        let largeGapCount = gapSum `mod` gapCount
        let changeLabels ref idx = when (ref /= delimRef) $ do
                cell <- readSTRef ref
                let lbl = labelSum
                              (label startCell)
                              (Label (idx * smallGap + min largeGapCount idx))
                writeSTRef ref (cell { label = lbl })
                changeLabels (next cell) (succ idx)
        changeLabels (next startCell) 1
    {-FIXME:
        We allow the number of cells to be larger than the square root of the
        number of possible labels as long as we find a sparse part in our circle
        of cells (since our order full condition is only true if the complete
        circle is congested). This should not influence correctness and probably
        also not time complexity, but we should check this more thoroughly.
    -}
    {-FIXME:
        We arrange the large and small gaps differently from Dietz and Sleator
        by putting all the large gaps at the beginning instead of distributing
        them over the relabeled area. However, this should not influence time
        complexity, as the complexity proof seems to only rely on the fact that
        gap sizes differ by at most 1. We should check this more thoroughly
        though.
    -}

    newBeforeCell :: CellRef s -> ST s (CellRef s)
    newBeforeCell ref = do
        cell <- readSTRef ref
        newAfterCell (prev cell)
