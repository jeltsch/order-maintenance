module Control.Monad.Trans.Order.Algorithm.DietzSleatorAmortizedLog (

    algorithm

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

algorithm :: Algorithm
algorithm = Algorithm rawAlgorithm

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

arenaSize :: LabelWord
arenaSize = 2 ^ 63

toLabel :: LabelWord -> Label
toLabel = Label . (`mod` arenaSize)

instance Num Label where

    Label word1 + Label word2 = toLabel (word1 + word2)

    Label word1 - Label word2 = toLabel (word1 - word2)

    Label word1 * Label word2 = toLabel (word1 * word2)

    negate (Label word) = toLabel (negate word)

    abs = id

    signum (Label word) = Label (signum word)

    fromInteger = toLabel . fromInteger

instance Real Label where

    toRational (Label word) = toRational word

instance Integral Label where

    Label word1 `quot` Label word2 = Label (word1 `quot` word2)

    Label word1 `rem` Label word2 = Label (word1 `rem` word2)

    Label word1 `quotRem` Label word2 = (Label quotWord, Label remWord) where

        (quotWord, remWord) = word1 `quotRem` word2

    div = quot

    mod = rem

    divMod = quotRem

    toInteger (Label word) = toInteger word

initialBaseLabel :: Label
initialBaseLabel = 0

rawAlgorithm :: RawAlgorithm DietzSleatorAmortizedLog s
rawAlgorithm = RawAlgorithm {
    newOrder        = fixST $
                      \ ref -> newSTRef $ Cell {
                         label = initialBaseLabel,
                         next  = ref,
                         prev  = ref
                      },
    compareElements = \ ref1 ref2 -> do
                          cell1 <- readSTRef ref1
                          cell2 <- readSTRef ref2
                          return $ compare (label cell1) (label cell2),
    newMinimum      = newAfterCell,
    newMaximum      = newBeforeCell,
    newAfter        = const . newAfterCell,
    newBefore       = const . newBeforeCell,
    delete          = \ ref _ -> do
                          cell <- readSTRef ref
                          modifySTRef
                              (prev cell)
                              (\ prevCell -> prevCell { next = next cell })
                          modifySTRef
                              (next cell)
                              (\ nextCell -> nextCell { prev = prev cell })
}

newAfterCell :: CellRef s -> ST s (CellRef s)
newAfterCell ref = do
    relabel ref
    cell <- readSTRef ref
    let nextRef = next cell
    nextCell <- readSTRef nextRef
    newRef <- newSTRef $ Cell {
        label = label cell + (label nextCell - label cell) `div` 2,
        next  = nextRef,
        prev  = ref
    }
    writeSTRef ref     (cell     { next = newRef })
    writeSTRef nextRef (nextCell { prev = newRef })
    return newRef

relabel :: CellRef s -> ST s ()
relabel startRef = do
    startCell <- readSTRef startRef
    let delimSearch ref gapCount = do
            cell <- readSTRef ref
            let gapSum = case label cell - label startCell of
                             Label diff | diff == 0 -> arenaSize
                                        | otherwise -> diff
            if gapSum <= gapCount ^ 2
                then delimSearch (next cell) (succ gapCount)
                else return (ref, gapSum, gapCount)
    (delimRef, gapSum, gapCount) <- delimSearch (next startCell) 1
    let smallGap = gapSum `div` gapCount
    let largeGapCount = gapSum `mod` gapCount
    let changeLabels ref idx = when (ref /= delimRef) $ do
            cell <- readSTRef ref
            let lbl = label startCell + Label (idx * smallGap + min largeGapCount idx)
            writeSTRef ref (cell { label = lbl })
            changeLabels (next cell) (succ idx)
    changeLabels (next startCell) 1

newBeforeCell :: CellRef s -> ST s (CellRef s)
newBeforeCell ref = do
    cell <- readSTRef ref
    newAfterCell (prev cell)
