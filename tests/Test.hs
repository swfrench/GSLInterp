module Main where

import Test.HUnit
import Math.GSLInterp
import qualified Data.Vector.Unboxed as V
import Data.Maybe (isJust)

testInit :: Maybe InterpStructPtr -> Test
testInit s = TestCase (assertBool "interpInit" (isJust s))

interpTestSuite :: IO ()
interpTestSuite = do
  -- create the test data
  let x  = 2.5
      nx = 21
      dx = 2.0 * pi / (nx - 1)
      xs = V.map (dx*) $ V.fromList [0..nx - 1]
      ys = V.map sin xs
  -- initialize the interpolant
  s <- interpInit xs ys
  -- run the tests
  runTestTT $ TestList [testInit s]
  case s of 
    Just p  -> interpFree p
    Nothing -> return ()

main :: IO ()
main = interpTestSuite
