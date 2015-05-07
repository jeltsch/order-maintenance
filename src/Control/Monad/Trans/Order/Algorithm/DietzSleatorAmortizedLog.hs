module Control.Monad.Trans.Order.Algorithm.DietzSleatorAmortizedLog (

    algorithm,
    algorithmWithSize

) where

-- Control

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Order.Algorithm.Type
import Control.Monad.Trans.Order.Raw

-- Data

import Data.Function
import Data.STRef
import Data.Word
import Data.Bits

algorithm :: Algorithm
algorithm = algorithmWithSize defaultSize

defaultSize :: Int
defaultSize = 63

algorithmWithSize :: Int -> Algorithm
algorithmWithSize size = Algorithm (rawAlgorithmWithSize size)

data DietzSleatorAmortizedLog

type instance OrderCell DietzSleatorAmortizedLog s = Cell s

type instance ElementCell DietzSleatorAmortizedLog s = Cell s

data Cell s = Cell {
                  label :: Label,
                  next  :: CellRef s,
                  prev  :: CellRef s
              }

type CellRef s = STRef s (Cell s)

newtype Label = Label LabelWord deriving (Eq, Ord, Enum, Show)

type LabelWord = Word64

labelWordSize :: Int
labelWordSize = 64

initialBaseLabel :: Label
initialBaseLabel = Label 0

rawAlgorithmWithSize :: Int -> RawAlgorithm DietzSleatorAmortizedLog s
rawAlgorithmWithSize size
    | size < 0 || size >= 64
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
              compareElements = \ ref1 ref2 -> do
                                    cell1 <- readSTRef ref1
                                    cell2 <- readSTRef ref2
                                    return $ compare (label cell1)
                                                     (label cell2),
              newMinimum      = newAfterCell,
              newMaximum      = newBeforeCell,
              newAfter        = const . newAfterCell,
              newBefore       = const . newBeforeCell,
              delete          = \ ref _ -> do
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
                    then delimSearch (next cell) (succ gapCount)
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

    newBeforeCell :: CellRef s -> ST s (CellRef s)
    newBeforeCell ref = do
        cell <- readSTRef ref
        newAfterCell (prev cell)
