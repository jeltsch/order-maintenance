module TestSuite (

    tests

) where

-- Control

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

-- Data

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Order.Algorithm (Algorithm, withRawAlgorithm)
import qualified Data.Order.Algorithm as Algorithm
import           Data.Order.Raw

-- Test

import Test.QuickCheck

-- Distribution

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

-- * Tests

tests :: IO [Test]
tests = return $ map (uncurry comparisonTest) [
            (dumb, dietzSleatorAmortizedLogWithSize14)
        ]

-- * Order computations

newtype OrderComp = OrderComp [OrderStmt]

initialID :: Int
initialID = 1

instance Show OrderComp where

    show (OrderComp stmts)
        | null stmts = "no statements"
        | otherwise  = str ++ concatMap (", " ++) strs where

            str : strs = zipWith showStmt stmts nextIds

            newElemCounts = map newElemCount stmts

            nextIds = scanl (+) initialID newElemCounts

data CompGenState = CompGenState (Set Int) Int

instance Arbitrary OrderComp where

    arbitrary = sized $ \ size -> do
                    len <- choose (0, size)
                    stmts <- evalStateT (replicateM len genStmt)
                                        (CompGenState Set.empty initialID)
                    return (OrderComp stmts)

    shrink (OrderComp stmts) = if null stmts
                                   then []
                                   else [OrderComp (init stmts)]

type ComparisonMatrix = Map (Int, Int) Ordering

runComp :: Algorithm -> OrderComp -> ComparisonMatrix
runComp alg comp = compMatrix where

    compMatrix = runST (withRawAlgorithm alg (\ rawAlg -> execComp rawAlg comp))

data CompExecState a s = CompExecState (ElementMap a s) Int

type ElementMap a s = Map Int (RawElement a s)

execComp :: RawAlgorithm a s -> OrderComp -> ST s ComparisonMatrix
execComp rawAlg (OrderComp stmts) = do
    rawOrder <- newOrder rawAlg
    let execStmts = mapM_ (execStmt rawAlg rawOrder) stmts
    let initState = CompExecState Map.empty initialID
    ((), CompExecState elemMap _) <- runStateT execStmts initState
    let idElemPairs = Map.toList elemMap
    let comparisonPair (id1, elem1) (id2, elem2) = do
            ordering <- compareElements rawAlg rawOrder elem1 elem2
            return ((id1, id2), ordering)
    comparisonPairs <- sequence $ liftM2 comparisonPair idElemPairs idElemPairs
    return $ Map.fromList comparisonPairs

data OrderStmt = NewMinimum
               | NewMaximum
               | NewAfter Int
               | NewBefore Int
               | Delete Int

newElemCount :: OrderStmt -> Int
newElemCount NewMinimum     = 1
newElemCount NewMaximum     = 1
newElemCount (NewAfter id)  = 1
newElemCount (NewBefore id) = 1
newElemCount (Delete id)    = 0

showStmt :: OrderStmt -> Int -> String
showStmt NewMinimum     = showNewStmt "newMinimum"
showStmt NewMaximum     = showNewStmt "newMaximum"
showStmt (NewAfter id)  = showNewStmt ("newAfter " ++ showElem id)
showStmt (NewBefore id) = showNewStmt ("newBefore " ++ showElem id)
showStmt (Delete id)    = const ("delete " ++ showElem id)

showNewStmt :: String -> Int -> String
showNewStmt base nextId = base ++ " -> " ++ showElem nextId

showElem :: Int -> String
showElem id = "x_" ++ show id

genStmt :: StateT CompGenState Gen OrderStmt
genStmt = do
    CompGenState liveIds nextId <- get
    let liveIdGen = elements (Set.toList liveIds)
    stmt <- lift $
            if Set.null liveIds
                then elements [NewMinimum, NewMaximum]
                else frequency [
                         (1, return NewMinimum),
                         (1, return NewMaximum),
                         (3, fmap NewAfter liveIdGen),
                         (3, fmap NewBefore liveIdGen),
                         (2, fmap Delete liveIdGen)
                     ]
    let newStmtIds = (Set.singleton nextId, Set.empty)
    let (newIds, deadIds) = case stmt of
                                NewMinimum  -> newStmtIds
                                NewMaximum  -> newStmtIds
                                NewAfter _  -> newStmtIds
                                NewBefore _ -> newStmtIds
                                Delete id   -> (Set.empty, Set.singleton id)
    put $ CompGenState ((liveIds `Set.union` newIds) `Set.difference` deadIds)
                       (nextId + Set.size newIds)
    return stmt

execStmt :: RawAlgorithm a s
         -> RawOrder a s
         -> OrderStmt
         -> StateT (CompExecState a s) (ST s) ()
execStmt rawAlg rawOrder = exec where

    exec NewMinimum     = execNew newMinimum
    exec NewMaximum     = execNew newMaximum
    exec (NewAfter id)  = execNewNeighbor newAfter id
    exec (NewBefore id) = execNewNeighbor newBefore id
    exec (Delete id)    = execDelete id

    execNew new = do
        CompExecState elemMap nextId <- get
        rawElem <- lift $ new rawAlg rawOrder
        put $ CompExecState (Map.insert nextId rawElem elemMap) (succ nextId)

    execNewNeighbor newNeighbor id = do
        CompExecState elemMap _ <- get
        let new rawAlg rawOrder = newNeighbor rawAlg rawOrder (elemMap Map.! id)
        execNew new

    execDelete id = do
        CompExecState elemMap nextId <- get
        lift $ delete rawAlg rawOrder (elemMap Map.! id)
        put $ CompExecState (Map.delete id elemMap) nextId

-- * Named algorithms

data NamedAlgorithm = NamedAlgorithm String Algorithm

dumb :: NamedAlgorithm
dumb = NamedAlgorithm "Dumb" Algorithm.dumb

dietzSleatorAmortizedLogWithSize14 :: NamedAlgorithm
dietzSleatorAmortizedLogWithSize14 = NamedAlgorithm name alg where

    name = "Dietz and Sleator O(log n) amortized time"

    alg = Algorithm.dietzSleatorAmortizedLogWithSize 14

-- * Test pattern

comparisonTest :: NamedAlgorithm -> NamedAlgorithm -> Test
comparisonTest (NamedAlgorithm name1 alg1)
               (NamedAlgorithm name2 alg2) = testProperty name prop where

    name = name1 ++ " vs. " ++ name2

    prop comp = runComp alg1 comp == runComp alg2 comp
