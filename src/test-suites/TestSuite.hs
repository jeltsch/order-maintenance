module TestSuite (

    tests

) where

-- Test

import Test.QuickCheck

-- Distribution

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

-- * Tests

tests :: IO [Test]
tests = return []
